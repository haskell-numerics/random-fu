{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs,
    BangPatterns
  #-}

-- |This module provides functions useful for implementing new 'MonadRandom'
-- and 'RandomSource' instances for state-abstractions containing 'StdGen'
-- values (the pure pseudorandom generator provided by the System.Random
-- module in the \"random\" package), as well as instances for some common
-- cases.
module Data.Random.Source.StdGen where

import Data.Random.Internal.Words
import Data.Random.Internal.Primitives
import Data.Random.Source
import System.Random
import Control.Monad.Prompt
import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S
import Data.StateRef
import Data.Word


instance (Monad m1, ModifyRef (Ref m2 StdGen) m1 StdGen) => RandomSource m1 (Ref m2 StdGen) where
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromRandomGenRef

instance (Monad m, ModifyRef (IORef   StdGen) m StdGen) => RandomSource m (IORef   StdGen) where
    {-# SPECIALIZE instance RandomSource IO (IORef StdGen) #-}
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromRandomGenRef

-- Note that this instance is probably a Bad Idea.  STM allows random variables
-- to interact in spooky quantum-esque ways - One transaction can 'retry' until
-- it gets a \"random\" answer it likes, which causes it to selectively consume 
-- entropy, biasing the supply from which other random variables will draw.
-- instance (Monad m, ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where
--     {-# SPECIALIZE instance RandomSource IO  (TVar StdGen) #-}
--     {-# SPECIALIZE instance RandomSource STM (TVar StdGen) #-}
--     supportedPrimsFrom _ _ = True
--     getSupportedRandomPrimFrom = getRandomPrimFromRandomGenRef

instance (Monad m, ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s StdGen) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s StdGen) #-}
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromRandomGenRef

getRandomPrimFromStdGenIO :: Prim a -> IO a
getRandomPrimFromStdGenIO PrimWord8 = do
    int <- randomRIO (0, 255) :: IO Int
    return (fromIntegral int)

getRandomPrimFromStdGenIO PrimWord32 = do
    int <- randomRIO (0, 0xffffffff)
    return (fromInteger int)

getRandomPrimFromStdGenIO PrimWord64 = do
    int <- randomRIO (0, 0xffffffffffffffff)
    return (fromInteger int)

-- based on reading the source of the "random" library's implementation, I do
-- not believe that the randomRIO (0,1) implementation for Double is capable of producing
-- the value 0.  Therefore, I'm not using it.  If this is an incorrect reading on
-- my part, or if this changes, then feel free to use the commented version.
-- Same goes for the other getRandomDouble... functions here.
getRandomPrimFromStdGenIO PrimDouble = liftM wordToDouble (getRandomPrimFromStdGenIO PrimWord64)
-- getRandomPrimFromStdGenIO PrimDouble = (0, 1)

getRandomPrimFromStdGenIO other = runPromptM getRandomPrimFromStdGenIO (decomposePrimWhere supported other)
    where 
        {-# INLINE supported #-}
        supported :: Prim a -> Bool
        supported PrimWord8  = True
        supported PrimWord32 = True
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
getRandomPrimFromRandomGenRef :: (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> Prim a -> m a
getRandomPrimFromRandomGenRef g PrimWord8
    = atomicModifyReference g $ \(!gen) -> case randomR (0,0xff) gen of
        (!w, !gen) -> (gen, fromIntegral (w :: Int))
    where 
        swap :: (Int, a) -> (a, Word8)
        swap (a,b) = (b,fromIntegral a)

getRandomPrimFromRandomGenRef g PrimWord32
    = atomicModifyReference g $ \(!gen) -> case randomR (0,0xffffffff) gen of
        (!w, !gen) -> (gen, fromInteger w)

getRandomPrimFromRandomGenRef g PrimWord64
    = atomicModifyReference g $ \(!gen) -> case randomR (0,0xffffffffffffffff) gen of
        (!w, !gen) -> (gen, fromInteger w)

getRandomPrimFromRandomGenRef g PrimDouble = liftM wordToDouble (getRandomPrimFromRandomGenRef g PrimWord64)
-- getRandomPrimFromRandomGenRef g PrimDouble = atomicModifyRef g (swap . randomR (0,1))
--     where swap (a,b) = (b,a)

getRandomPrimFromRandomGenRef g other = runPromptM (getRandomPrimFromRandomGenRef g) (decomposePrimWhere supported other)
    where 
        {-# INLINE supported #-}
        supported :: Prim a -> Bool
        supported PrimWord8  = True
        supported PrimWord32  = True
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False


-- |Similarly, @getRandomWordFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
getRandomPrimFromRandomGenState :: (RandomGen g, MonadState g m) => Prim a -> m a
getRandomPrimFromRandomGenState PrimWord8 = do
    g <- get
    case randomR (0, 255 :: Int) g of
        (i,g) -> do
            put g
            return (fromIntegral i)

getRandomPrimFromRandomGenState PrimWord64 = do
    g <- get
    case randomR (0, 0xffffffffffffffff) g of
        (i,g) -> do
            put g
            return (fromInteger i)

getRandomPrimFromRandomGenState PrimDouble = liftM wordToDouble (getRandomPrimFromRandomGenState PrimWord64)
-- getRandomPrimFromRandomGenState PrimDouble = do
--     g <- get
--     case randomR (0, 1) g of
--         (x,g) -> do
--             put g
--             return x

getRandomPrimFromRandomGenState other = runPromptM getRandomPrimFromRandomGenState (decomposePrimWhere supported other)
    where 
        supported :: Prim a -> Bool
        supported PrimWord8  = True
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False


instance MonadRandom (State StdGen) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromRandomGenState

instance Monad m => MonadRandom (StateT StdGen m) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromRandomGenState

instance MonadRandom (S.State StdGen) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromRandomGenState

instance Monad m => MonadRandom (S.StateT StdGen m) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromRandomGenState

