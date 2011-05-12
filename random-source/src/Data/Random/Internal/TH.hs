{-# LANGUAGE TemplateHaskell, GADTs #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.Random.Internal.TH (monadRandom, randomSource) where

import Data.Bits
import Data.Generics
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Random.Internal.Source
import Data.Random.Internal.Words
import Language.Haskell.TH
import qualified Language.Haskell.TH.FlexibleDefaults as FD

import Control.Monad.Reader

data Method
    = GetPrim
    | GetWord8
    | GetWord16
    | GetWord32
    | GetWord64
    | GetDouble
    | GetNByteInteger
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

allMethods :: [Method]
allMethods = [minBound .. maxBound]

data Context
    = Generic
    | RandomSource
    | MonadRandom
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

methodNameBase :: Context -> Method -> String
methodNameBase c n = nameBase (methodName c n)

methodName :: Context -> Method -> Name
methodName Generic      GetPrim          = mkName "getPrim"
methodName Generic      GetWord8         = mkName "getWord8"
methodName Generic      GetWord16        = mkName "getWord16"
methodName Generic      GetWord32        = mkName "getWord32"
methodName Generic      GetWord64        = mkName "getWord64"
methodName Generic      GetDouble        = mkName "getDouble"
methodName Generic      GetNByteInteger  = mkName "getNByteInteger"
methodName RandomSource GetPrim          = 'getRandomPrimFrom
methodName RandomSource GetWord8         = 'getRandomWord8From
methodName RandomSource GetWord16        = 'getRandomWord16From
methodName RandomSource GetWord32        = 'getRandomWord32From
methodName RandomSource GetWord64        = 'getRandomWord64From
methodName RandomSource GetDouble        = 'getRandomDoubleFrom
methodName RandomSource GetNByteInteger  = 'getRandomNByteIntegerFrom
methodName MonadRandom  GetPrim          = 'getRandomPrim
methodName MonadRandom  GetWord8         = 'getRandomWord8
methodName MonadRandom  GetWord16        = 'getRandomWord16
methodName MonadRandom  GetWord32        = 'getRandomWord32
methodName MonadRandom  GetWord64        = 'getRandomWord64
methodName MonadRandom  GetDouble        = 'getRandomDouble
methodName MonadRandom  GetNByteInteger  = 'getRandomNByteInteger

isMethodName :: Context -> Name -> Bool
isMethodName c n = isJust (nameToMethod c n)

nameToMethod :: Context -> Name -> Maybe Method
nameToMethod c name
    = lookup name
        [ (n, m) 
        | m <- allMethods
        , let n = methodName c m
        ]


-- 'Context'-sensitive version of the FlexibleDefaults DSL
scoreBy :: (a -> b) -> ReaderT Context (FD.Defaults a) t -> ReaderT Context (FD.Defaults b) t
scoreBy f = mapReaderT (FD.scoreBy f)

method :: Method -> ReaderT Context (FD.Function s) t -> ReaderT Context (FD.Defaults s) t
method m f = do
    c <- ask
    mapReaderT (FD.function (methodNameBase c m)) f

requireMethod :: Method -> ReaderT Context (FD.Defaults s) ()
requireMethod m = do
    c <- ask
    lift (FD.requireFunction (methodNameBase c m))

implementation :: ReaderT Context (FD.Implementation s) (Q [Dec]) -> ReaderT Context (FD.Function s) ()
implementation = mapReaderT FD.implementation

score :: s -> ReaderT Context (FD.Implementation s) ()
score = lift . FD.score

cost :: Num s => s -> ReaderT Context (FD.Implementation s) ()
cost = lift . FD.cost

dependsOn :: Method -> ReaderT Context (FD.Implementation s) ()
dependsOn m = do
    c <- ask
    lift (FD.dependsOn (methodNameBase c m))

replace :: (a -> Maybe a) -> (a -> a)
replace = ap fromMaybe

replaceMethodName :: (Method -> Name) -> Name -> Name
replaceMethodName f = replace (fmap f . nameToMethod Generic)

changeContext :: Context -> Context -> Name -> Name
changeContext c1 c2 = replace (fmap (methodName c2) . nameToMethod c1)

-- map all occurrences of generic method names to the proper local ones
-- and introduce a 'src' parameter where needed if the Context is RandomSource
specialize :: Monad m => Q [Dec] -> ReaderT Context m (Q [Dec])
specialize decQ = do
    c <- ask
    let specializeDec = everywhere (mkT (changeContext Generic c))
    if c == RandomSource
        then return $ do
                src <- newName "_src"
                decs <- decQ
                return (map (addSrcParam src) . specializeDec $ decs)
        else return (fmap specializeDec decQ)

addSrcParam :: Name -> Dec -> Dec
addSrcParam src
    = everywhere (mkT expandDecs) 
    . everywhere (mkT expandExps)
    where
        srcP = VarP src
        srcE = VarE src
        
        expandDecs (ValD (VarP n) body decs)
            | isMethodName RandomSource n
            = FunD n [Clause [srcP] body decs]
        expandDecs (FunD n clauses)
            | isMethodName RandomSource n
            = FunD n [Clause (srcP : ps) body decs | Clause ps body decs <- clauses]
        
        expandDecs other = other
        
        expandExps e@(VarE n)
            | isMethodName RandomSource n   = AppE e srcE
        expandExps other = other

-- dummy expressions which will be remapped by 'specialize'
dummy :: Method -> ExpQ
dummy = return . VarE . methodName Generic

getPrim, getWord8, getWord16, 
    getWord32, getWord64, getDouble, 
    getNByteInteger :: ExpQ
getPrim             = dummy GetPrim
getWord8            = dummy GetWord8
getWord16           = dummy GetWord16
getWord32           = dummy GetWord32
getWord64           = dummy GetWord64
getDouble           = dummy GetDouble
getNByteInteger     = dummy GetNByteInteger

intIs64 :: Bool
intIs64 = toInteger (maxBound :: Int) > 2^32

-- The defaulting rules for RandomSource and MonadRandom.  Costs are rates of
-- entropy waste (bits discarded per bit requested) plus the occasional ad-hoc
-- penalty where it seems appropriate.

-- TODO: figure out a clean way to break these up for individual testing.
-- Also analyze to see which of these can never be selected (I suspect that set is non-empty)
defaults :: Context -> FD.Defaults (Sum Double) ()
defaults = runReaderT $
    scoreBy Sum $ do
        method GetPrim $ do
            implementation $ do
                mapM_ dependsOn (allMethods \\ [GetPrim])
                specialize
                    [d| getPrim PrimWord8               = $getWord8
                        getPrim PrimWord16              = $getWord16
                        getPrim PrimWord32              = $getWord32
                        getPrim PrimWord64              = $getWord64
                        getPrim PrimDouble              = $getDouble
                        getPrim (PrimNByteInteger n)    = $getNByteInteger n
                     |]
        
        scoreBy (/8) $
            method GetWord8 $ do
                implementation $ do
                    dependsOn GetPrim
                    specialize [d| getWord8 = $getPrim PrimWord8 |]
                
                implementation $ do
                    cost 1
                    dependsOn GetNByteInteger
                    specialize [d| getWord8 = liftM fromInteger ($getNByteInteger 1) |]
                
                implementation $ do
                    cost 8
                    dependsOn GetWord16
                    specialize [d| getWord8 = liftM fromIntegral $getWord16 |]
                
                implementation $ do
                    cost 24
                    dependsOn GetWord32
                    specialize [d| getWord8 = liftM fromIntegral $getWord32 |]
                
                implementation $ do
                    cost 56
                    dependsOn GetWord64
                    specialize [d| getWord8 = liftM fromIntegral $getWord64 |]
                
                implementation $ do
                    cost 64
                    dependsOn GetDouble
                    specialize [d| getWord8 = liftM (truncate . (256*)) $getDouble |]
                
        scoreBy (/16) $
            method GetWord16 $ do
                implementation $ do
                    dependsOn GetPrim
                    specialize [d| getWord16 = $getPrim PrimWord16 |]
                
                implementation $ do
                    cost 1
                    dependsOn GetNByteInteger
                    specialize [d| getWord16 = liftM fromInteger ($getNByteInteger 2) |]
                
                implementation $ do
                    dependsOn GetWord8
                    specialize 
                        [d|
                            getWord16 = do
                                a <- $getWord8
                                b <- $getWord8
                                return (buildWord16 a b)
                         |]
                
                implementation $ do
                    cost 16
                    dependsOn GetWord32
                    specialize [d| getWord16 = liftM fromIntegral $getWord32 |]
                
                implementation $ do
                    cost 48
                    dependsOn GetWord64
                    specialize [d| getWord16 = liftM fromIntegral $getWord64 |]
                
                implementation $ do
                    cost 64
                    dependsOn GetDouble
                    specialize [d| getWord16 = liftM (truncate . (65536*)) $getDouble |]
        
        scoreBy (/32) $
            method GetWord32 $ do
                implementation $ do
                    dependsOn GetPrim
                    specialize [d| getWord32 = $getPrim PrimWord32 |]
                
                implementation $ do
                    cost 1
                    dependsOn GetNByteInteger
                    specialize [d| getWord32 = liftM fromInteger ($getNByteInteger 4) |]
                
                implementation $ do
                    cost 0.1
                    dependsOn GetWord8
                    specialize 
                        [d|
                            getWord32 = do
                                a <- $getWord8
                                b <- $getWord8
                                c <- $getWord8
                                d <- $getWord8
                                return (buildWord32 a b c d)
                         |]
                
                implementation $ do
                    dependsOn GetWord16
                    specialize 
                        [d|
                            getWord32 = do
                                a <- $getWord16
                                b <- $getWord16
                                return (buildWord32' a b)
                         |]
                
                implementation $ do
                    cost 32
                    dependsOn GetWord64
                    specialize [d| getWord32 = liftM fromIntegral $getWord64 |]
                
                implementation $ do
                    cost 64
                    dependsOn GetDouble
                    specialize [d| getWord32 = liftM (truncate . (4294967296*)) $getDouble |]
        
        scoreBy (/64) $
            method GetWord64 $ do
                implementation $ do
                    dependsOn GetPrim
                    specialize [d| getWord64 = $getPrim PrimWord64 |]
                
                implementation $ do
                    cost 1
                    dependsOn GetNByteInteger
                    specialize [d| getWord64 = liftM fromInteger ($getNByteInteger 8) |]
                
                implementation $ do
                    cost 0.2
                    dependsOn GetWord8
                    specialize 
                        [d|
                            getWord64 = do
                                a <- $getWord8
                                b <- $getWord8
                                c <- $getWord8
                                d <- $getWord8
                                e <- $getWord8
                                f <- $getWord8
                                g <- $getWord8
                                h <- $getWord8
                                return (buildWord64 a b c d e f g h)
                         |]
                
                implementation $ do
                    cost 0.1
                    dependsOn GetWord16
                    specialize 
                        [d|
                            getWord64 = do
                                a <- $getWord16
                                b <- $getWord16
                                c <- $getWord16
                                d <- $getWord16
                                return (buildWord64' a b c d)
                         |]
                
                implementation $ do
                    dependsOn GetWord32
                    specialize 
                        [d|
                            getWord64 = do
                                a <- $getWord32
                                b <- $getWord32
                                return (buildWord64'' a b)
                         |]
        
        scoreBy (/52) $
            method GetDouble $ do
                implementation $ do
                    dependsOn GetPrim
                    specialize [d| getDouble = $getPrim PrimDouble |]
                
                implementation $ do
                    cost 12
                    dependsOn GetWord64
                    specialize 
                        [d|
                            getDouble = do
                                w <- $getWord64
                                return (wordToDouble w)
                         |]
        
        method GetNByteInteger $ do
            implementation $ do
                dependsOn GetPrim
                specialize [d| getNByteInteger n = $getPrim (PrimNByteInteger n) |]
            
            implementation $ do
                when intIs64 (cost 1e-2)
                dependsOn GetWord8
                dependsOn GetWord16
                dependsOn GetWord32
                specialize
                    [d|
                        getNByteInteger 1 = do
                            x <- $getWord8
                            return $! toInteger x
                        getNByteInteger 2 = do
                            x <- $getWord16
                            return $! toInteger x
                        getNByteInteger 4 = do
                            x <- $getWord32
                            return $! toInteger x
                        getNByteInteger np4
                            | np4 > 4 = do
                                let n = np4 - 4
                                x <- $getWord32
                                y <- $(dummy GetNByteInteger) n
                                return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                        getNByteInteger np2
                            | np2 > 2 = do
                                let n = np2 - 2
                                x <- $getWord16
                                y <- $(dummy GetNByteInteger) n
                                return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                        getNByteInteger _ = return 0
                      |]
                    
            implementation $ do
                when (not intIs64) (cost 1e-2)
                dependsOn GetWord8
                dependsOn GetWord16
                dependsOn GetWord32
                dependsOn GetWord64
                specialize
                    [d|
                        getNByteInteger 1 = do
                            x <- $getWord8
                            return $! toInteger x
                        getNByteInteger 2 = do
                            x <- $getWord16
                            return $! toInteger x
                        getNByteInteger 4 = do
                            x <- $getWord32
                            return $! toInteger x
                        getNByteInteger 8 = do
                            x <- $getWord64
                            return $! toInteger x
                        getNByteInteger np8
                            | np8 > 8 = do
                                let n = np8 - 8
                                x <- $getWord64
                                y <- $(dummy GetNByteInteger) n
                                return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                        getNByteInteger np4
                            | np4 > 4 = do
                                let n = np4 - 4
                                x <- $getWord32
                                y <- $(dummy GetNByteInteger) n
                                return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                        getNByteInteger np2
                            | np2 > 2 = do
                                let n = np2 - 2
                                x <- $getWord16
                                y <- $(dummy GetNByteInteger) n
                                return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                        getNByteInteger _ = return 0
                      |]
                    

randomSource :: Q [Dec] -> Q [Dec]
randomSource = FD.withDefaults (defaults RandomSource)

monadRandom :: Q [Dec] -> Q [Dec]
monadRandom = FD.withDefaults (defaults MonadRandom)

-- -- This is nice in theory, but under GHC 7 it never typechecks; without generalizing the let-bound
-- -- functions, it gets absurd errors like "cannot match 'm Int' with 'IO t'".  Probably need
-- -- to mechanically specialize the supplied signature to create a signature for every other
-- -- let-bound function.
-- primFunction :: Q Type -> Q [Dec] -> ExpQ
-- primFunction getPrimType decsQ = do
--     getPrimSig <- sigD (mkName (methodName Generic GetPrim)) getPrimType
--     decs <- decsQ >>= FD.implementDefaults (defaults Generic)
--     f <- getPrim
--     return (LetE (getPrimSig : decs) f)
