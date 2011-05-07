{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes #-}
module Data.Random.Internal.TH (monadRandom, randomSource) where

import Control.Monad
import Data.Char
import Data.Generics
import Data.List
import Data.Maybe
import Data.Random.Internal.Source
import Data.Random.Internal.Prim
import Data.Word
import Language.Haskell.TH
    ( Q
    , Name, mkName, newName, nameBase, nameModule
    , Exp(..), Match(..), Pat(..), Lit(..)
    , Dec(..), Body(..), Clause(..)
    , Type(..), TyVarBndr(..), Cxt)

data Method
    = GetWord8
    | GetWord16
    | GetWord32
    | GetWord64
    | GetDouble
    | GetNByteInteger
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

allMethods :: [Method]
allMethods = [minBound .. maxBound]

allMethodNames :: [Name]
allMethodNames = map methodName allMethods

methodName :: Method -> Name
methodName GetWord8         = mkName "getWord8"
methodName GetWord16        = mkName "getWord16"
methodName GetWord32        = mkName "getWord32"
methodName GetWord64        = mkName "getWord64"
methodName GetDouble        = mkName "getDouble"
methodName GetNByteInteger  = mkName "getNByteInteger"

nameToMethod :: Name -> Maybe Method
nameToMethod name
    = lookup name
        [ (n, m) 
        | m <- allMethods
        , let n = methodName m
        ]

maybeForall :: Cxt -> [Name] -> Type -> Type
maybeForall _   [] = id
maybeForall cxt vs = ForallT (map PlainTV vs) cxt

arrow :: Type -> Type -> Type
arrow x y = AppT (AppT ArrowT x) y

arrows :: [Type] -> Type
arrows = foldr1 arrow

implementedMethods :: [Dec] -> [Method]
implementedMethods decls = nub $ catMaybes
    [ nameToMethod name
    | decl <- decls
    , name <- declaredValueNames decl
    ]

declaredValueNames :: Dec -> [Name]
declaredValueNames (FunD n _)    = [n]
declaredValueNames (ValD p _ _)  = matchedNames p
declaredValueNames _ = []

matchedNames :: Pat -> [Name]
matchedNames (VarP n)           = [n]
matchedNames (TupP ps)          = concatMap matchedNames ps
matchedNames (InfixP p1 _ p2)   = matchedNames p1 ++ matchedNames p2
matchedNames (TildeP p)         = matchedNames p
matchedNames (BangP p)          = matchedNames p
matchedNames (AsP n p)          = n : matchedNames p
matchedNames (RecP _ fs)        = concatMap (matchedNames . snd) fs
matchedNames (ListP ps)         = concatMap matchedNames ps
matchedNames (SigP p _)         = matchedNames p
matchedNames (ViewP _ p)        = matchedNames p
matchedNames _                  = []

monadRandomDeclSigs :: Cxt -> Type -> [Name] -> [Method] -> [Dec]
monadRandomDeclSigs  cxt m   vs methods = declSigsBy id        cxt m vs methods
randomSourceDeclSigs :: Cxt -> Type -> Type -> [Name] -> [Method] -> [Dec]
randomSourceDeclSigs cxt m s vs methods = declSigsBy (arrow s) cxt m vs methods

declSigsBy :: (Type -> Type)
             -> Cxt
             -> Type
             -> [Name]
             -> [Method]
             -> [Dec]
declSigsBy f cxt m vs methods =
    [ SigD n . maybeForall cxt vs . f . methodSig $ method
    | method <- methods
    , let n = methodName method
    ] where
        methodSig GetWord8         = arrows [AppT m (ConT ''Word8)]
        methodSig GetWord16        = arrows [AppT m (ConT ''Word16)]
        methodSig GetWord32        = arrows [AppT m (ConT ''Word32)]
        methodSig GetWord64        = arrows [AppT m (ConT ''Word64)]
        methodSig GetDouble        = arrows [AppT m (ConT ''Double)]
        methodSig GetNByteInteger  = arrows [ConT ''Int, AppT m (ConT ''Integer)]

primPat :: Bool -> Method -> Pat
primPat _    GetWord8         = ConP 'PrimWord8        []
primPat _    GetWord16        = ConP 'PrimWord16       []
primPat _    GetWord32        = ConP 'PrimWord32       []
primPat _    GetWord64        = ConP 'PrimWord64       []
primPat _    GetDouble        = ConP 'PrimDouble       []
primPat used GetNByteInteger  = ConP 'PrimNByteInteger [if used then VarP (mkName "i") else WildP]

primPatArgs :: Method -> [Exp]
primPatArgs GetWord8         = []
primPatArgs GetWord16        = []
primPatArgs GetWord32        = []
primPatArgs GetWord64        = []
primPatArgs GetDouble        = []
primPatArgs GetNByteInteger  = [VarE (mkName "i")]

generalizeVars :: Data a => a -> a
generalizeVars = everywhere (mkT generalizeVarName)

tyVars :: Data a => a -> [Name]
tyVars = nub . everything (++) (mkQ [] (\it -> if isTyVar it then [it] else []))

isTyVar :: Name -> Bool
isTyVar n = 
    let base = nameBase n
     in case nameModule n of
            Nothing 
                | not (null base) && isLower (head base)
                -> True
            _  -> False

generalizeVarName :: Name -> Name
generalizeVarName n 
    | isTyVar n = mkName (nameBase n)
    | otherwise = n

-- reify a (Method -> Bool) predicate to a (Prim a -> Bool) predicate.
mkSupportedD :: Name -> [Method] -> Q [Dec]
mkSupportedD name methods = do
    a    <- newName "a"
    let supportedT = ForallT [PlainTV a] [] 
            (arrows [AppT (ConT ''Prim) (VarT a), ConT ''Bool])
    
    return
        [ SigD name supportedT
        , FunD name 
            [ Clause [primPat False m] (NormalB implemented) []
            | m <- allMethods
            , let implemented 
                    | m `elem` methods  = ConE 'True
                    | otherwise         = ConE 'False
            ]
        ]

mkGetPrimFromD :: Name -> Cxt -> Type -> Type -> [Dec] -> Q [Dec]
mkGetPrimFromD name cxt m s decls = do
    a    <- newName "a"
    let msVars = nub (tyVars m ++ tyVars s)
        
        getPrimFromT = ForallT (map PlainTV (a : msVars)) cxt 
            (arrows [s, AppT (ConT ''Prim) (VarT a), AppT m (VarT a)])
    
    p    <- newName "p"
    src  <- newName "src"
    
    let methods = implementedMethods decls
        exhaustive = all (`elem` methods) allMethods
        makeExhaustive
            | exhaustive = id
            | otherwise  = (++ [defaultCase])
        thErrorString = "Error in Template Haskell function " ++ show 'randomSource
        defaultCase = Match WildP (NormalB (AppE (VarE 'error) (LitE (StringL thErrorString)))) []
    
    return
        [ SigD name getPrimFromT
        , FunD name 
            [ Clause [VarP src, VarP p] 
                (NormalB (CaseE (VarE p)
                    ( makeExhaustive
                        [ Match (primPat True method)
                            (NormalB (foldl AppE (VarE (methodName method)) (VarE src : primPatArgs method)))
                            []
                            | method <- methods
                        ])))
                (randomSourceDeclSigs cxt m s msVars methods ++ decls)
            ]
        ]

mkGetPrimD :: Name -> Cxt -> Type -> [Dec] -> Q [Dec]
mkGetPrimD name cxt m decls = do
    a    <- newName "a"
    let mVars = tyVars m
        
        getPrimFromT = ForallT (map PlainTV (a : mVars)) cxt 
            (arrows [AppT (ConT ''Prim) (VarT a), AppT m (VarT a)])
    
    p    <- newName "p"
    
    let methods = implementedMethods decls
        exhaustive = all (`elem` methods) allMethods
        makeExhaustive
            | exhaustive = id
            | otherwise  = (++ [defaultCase])
        thErrorString = "Error in Template Haskell function " ++ show 'monadRandom
        defaultCase = Match WildP (NormalB (AppE (VarE 'error) (LitE (StringL thErrorString)))) []
    
    return
        [ SigD name getPrimFromT
        , FunD name 
            [ Clause [VarP p] 
                (NormalB (CaseE (VarE p)
                    ( makeExhaustive [ Match (primPat True method)
                            (NormalB (foldl AppE (VarE (methodName method)) (primPatArgs method)))
                            []
                        | method <- methods
                        ])))
                (monadRandomDeclSigs cxt m mVars methods ++ decls)
            ]
        ]

monadRandom :: Q [Dec] -> Q [Dec] -> Q [Dec]
monadRandom instQ declsQ = do
    inst <- instQ
    (cxt, ty, m) <- case inst of
            [InstanceD cxt ty@(AppT (ConT cls) m) []]
                | nameBase cls == "MonadRandom" -> return (generalizeVars cxt, generalizeVars ty, generalizeVars m)
            _ -> fail "monadRandom: first parameter must consist solely of an empty MonadRandom instance declaration"
    
    decls <- declsQ
    let methods = implementedMethods decls
    when (null methods) $
        fail ("monadRandom: second parameter must define at least one of: " ++ show allMethodNames)
    
    supported   <- newName "supported"
    supportedD  <- mkSupportedD supported methods
    
    getPrim     <- newName "getPrim"
    getPrimD    <- mkGetPrimD getPrim cxt m decls
    
    let getRandomPrimName = 'getRandomPrim
        getRandomPrimD = ValD (VarP getRandomPrimName)
            (NormalB (foldl1 AppE [VarE 'getPrimWhere, VarE supported, VarE getPrim]))
            (supportedD ++ getPrimD)
    
    return [InstanceD cxt ty [getRandomPrimD]]

randomSource :: Q [Dec] -> Q [Dec] -> Q [Dec]
randomSource instQ declsQ = do
    inst <- instQ
    (cxt, ty, m, s) <- case inst of
            [InstanceD cxt ty@(AppT (AppT (ConT cls) m) s) []]
                | nameBase cls == "RandomSource" -> return (generalizeVars cxt, generalizeVars ty, generalizeVars m, generalizeVars s)
            _ -> fail "randomSource: first parameter must consist solely of an empty RandomSource instance declaration"
    
    decls <- declsQ
    let methods = implementedMethods decls
    when (null methods) $
        fail ("randomSource: second parameter must define at least one of: " ++ show allMethodNames)
    
    supported       <- newName "supported"
    supportedD      <- mkSupportedD supported methods
    
    getPrimFrom     <- newName "getPrimFrom"
    getPrimFromD    <- mkGetPrimFromD getPrimFrom cxt m s decls
    
    src  <- newName "src"
    let getRandomPrimFromName = 'getRandomPrimFrom
        getRandomPrimFromD = FunD getRandomPrimFromName
            [ Clause [VarP src]
                (NormalB (AppE (AppE (VarE 'getPrimWhere) (VarE supported)) (AppE (VarE getPrimFrom) (VarE src))))
                (supportedD ++ getPrimFromD)
            ]
    
    return [InstanceD cxt ty [getRandomPrimFromD]]

