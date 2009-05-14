{-
 -      ``Data/Random/Source/StdGen''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances
  #-}

module Data.Random.Source.StdGen where

import Data.Random.Source
import System.Random
import Control.Monad
import Control.Monad.State
import Data.StateRef
import Data.Word

instance (ModifyRef (IORef   StdGen) m StdGen) => RandomSource m (IORef   StdGen) where
    getRandomByteFrom = getRandomByteFromRandomGenRef
    getRandomWordFrom = getRandomWordFromRandomGenRef
instance (ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where
    getRandomByteFrom = getRandomByteFromRandomGenRef
    getRandomWordFrom = getRandomWordFromRandomGenRef
instance (ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where
    getRandomByteFrom = getRandomByteFromRandomGenRef
    getRandomWordFrom = getRandomWordFromRandomGenRef

getRandomByteFromStdGenIO :: IO Word8
getRandomByteFromStdGenIO = do
    int <- randomRIO (0, 255) :: IO Int
    return (fromIntegral int)

getRandomWordFromStdGenIO :: IO Word64
getRandomWordFromStdGenIO = do
    int <- randomRIO (0, 0xffffffffffffffff)
    return (fromInteger int)

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
--
-- For example, if @x :: TVar StdGen@, @getRandomBytesFromRandomGenRef x@ can be
-- used as a 'RandomSource' in 'IO', 'STM', or any monad which is an instance
-- of 'MonadIO'.  It's generally probably better to use
-- 'getRandomWordsFromRandomGenRef' though, as this one is likely to throw
-- away a lot of perfectly good entropy.
getRandomByteFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> m Word8
getRandomByteFromRandomGenRef g = atomicModifyRef g (swap . randomR (0,255))
    where 
        swap :: (Int, a) -> (a, Word8)
        swap (a,b) = (b,fromIntegral a)

getRandomWordFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> m Word64
getRandomWordFromRandomGenRef g = atomicModifyRef g (swap . randomR (0,0xffffffffffffffff))
    where swap (a,b) = (b,fromInteger a)

-- |Similarly, @getRandomWordsFromRandomGenState x@ can be used in any \"state\"
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


instance MonadRandom (State StdGen) where
    getRandomByte  = getRandomByteFromRandomGenState
    getRandomWord  = getRandomWordFromRandomGenState

instance Monad m => MonadRandom (StateT StdGen m) where
    getRandomByte  = getRandomByteFromRandomGenState
    getRandomWord  = getRandomWordFromRandomGenState
