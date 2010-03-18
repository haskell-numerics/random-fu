{-
 -      ``Data/Random/Source/StdGen''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs
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
import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S
import Data.StateRef
import Data.Word

instance (Monad m1, ModifyRef (Ref m2 StdGen) m1 StdGen) => RandomSource m1 (Ref m2 StdGen) where
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromRandomGenRef
    getRandomWordFrom   = getRandomWordFromRandomGenRef
    getRandomDoubleFrom = getRandomDoubleFromRandomGenRef

instance (Monad m, ModifyRef (IORef   StdGen) m StdGen) => RandomSource m (IORef   StdGen) where
    {-# SPECIALIZE instance RandomSource IO (IORef StdGen) #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromRandomGenRef
    getRandomWordFrom   = getRandomWordFromRandomGenRef
    getRandomDoubleFrom = getRandomDoubleFromRandomGenRef
instance (Monad m, ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where
    {-# SPECIALIZE instance RandomSource IO  (TVar StdGen) #-}
    {-# SPECIALIZE instance RandomSource STM (TVar StdGen) #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromRandomGenRef
    getRandomWordFrom   = getRandomWordFromRandomGenRef
    getRandomDoubleFrom = getRandomDoubleFromRandomGenRef
instance (Monad m, ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s StdGen) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s StdGen) #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromRandomGenRef
    getRandomWordFrom   = getRandomWordFromRandomGenRef
    getRandomDoubleFrom = getRandomDoubleFromRandomGenRef

getRandomByteFromStdGenIO :: IO Word8
getRandomByteFromStdGenIO = do
    int <- randomRIO (0, 255) :: IO Int
    return (fromIntegral int)

getRandomWordFromStdGenIO :: IO Word64
getRandomWordFromStdGenIO = do
    int <- randomRIO (0, 0xffffffffffffffff)
    return (fromInteger int)

-- based on reading the source of the "random" library's implementation, I do
-- not believe that the randomRIO (0,1) implementation for Double is capable of producing
-- the value 0.  Therefore, I'm not using it.  If this is an incorrect reading on
-- my part, or if this changes, then feel free to use the commented version.
-- Same goes for the other getRandomDouble... functions here.
getRandomDoubleFromStdGenIO :: IO Double
getRandomDoubleFromStdGenIO = liftM wordToDouble getRandomWordFromStdGenIO
-- getRandomDoubleFromStdGenIO = randomRIO (0, 1)

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
--
-- For example, if @x :: TVar StdGen@, @getRandomByteFromRandomGenRef x@ can be
-- used as a 'RandomSource' in 'IO', 'STM', or any monad which is an instance
-- of 'MonadIO'.  It's generally probably better to use
-- 'getRandomWordFromRandomGenRef' though, as this one is likely to throw
-- away a lot of perfectly good entropy.  Better still is to use these 3 functions
-- together to create a 'RandomSource' instance for the reference you're using,
-- if one does not already exist.
getRandomByteFromRandomGenRef :: (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> m Word8
getRandomByteFromRandomGenRef g = atomicModifyReference g (swap . randomR (0,255))
    where 
        swap :: (Int, a) -> (a, Word8)
        swap (a,b) = (b,fromIntegral a)

getRandomWordFromRandomGenRef :: (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> m Word64
getRandomWordFromRandomGenRef g = atomicModifyReference g (swap . randomR (0,0xffffffffffffffff))
    where swap (a,b) = (b,fromInteger a)

getRandomDoubleFromRandomGenRef :: (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> m Double
getRandomDoubleFromRandomGenRef g = liftM wordToDouble (getRandomWordFromRandomGenRef g)
-- getRandomDoubleFromRandomGenRef g = atomicModifyRef g (swap . randomR (0,1))
--     where swap (a,b) = (b,a)

-- |Similarly, @getRandomWordFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
getRandomByteFromRandomGenState :: (RandomGen g, MonadState g m) => m Word8
getRandomByteFromRandomGenState = do
    g <- get
    case randomR (0, 255 :: Int) g of
        (i,g) -> do
            put g
            return (fromIntegral i)

getRandomWordFromRandomGenState :: (RandomGen g, MonadState g m) => m Word64
getRandomWordFromRandomGenState = do
    g <- get
    case randomR (0, 0xffffffffffffffff) g of
        (i,g) -> do
            put g
            return (fromInteger i)

getRandomDoubleFromRandomGenState :: (RandomGen g, MonadState g m) => m Double
getRandomDoubleFromRandomGenState = liftM wordToDouble getRandomWordFromRandomGenState
-- getRandomDoubleFromRandomGenState = do
--     g <- get
--     case randomR (0, 1) g of
--         (x,g) -> do
--             put g
--             return x


instance MonadRandom (State StdGen) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromRandomGenState
    getRandomWord   = getRandomWordFromRandomGenState
    getRandomDouble = getRandomDoubleFromRandomGenState

instance Monad m => MonadRandom (StateT StdGen m) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromRandomGenState
    getRandomWord   = getRandomWordFromRandomGenState
    getRandomDouble = getRandomDoubleFromRandomGenState

instance MonadRandom (S.State StdGen) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromRandomGenState
    getRandomWord   = getRandomWordFromRandomGenState
    getRandomDouble = getRandomDoubleFromRandomGenState

instance Monad m => MonadRandom (S.StateT StdGen m) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromRandomGenState
    getRandomWord   = getRandomWordFromRandomGenState
    getRandomDouble = getRandomDoubleFromRandomGenState
