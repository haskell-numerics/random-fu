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
    getRandomBytesFrom = getRandomBytesFromRandomGenRef
    getRandomWordsFrom = getRandomWordsFromRandomGenRef
instance (ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where
    getRandomBytesFrom = getRandomBytesFromRandomGenRef
    getRandomWordsFrom = getRandomWordsFromRandomGenRef
instance (ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where
    getRandomBytesFrom = getRandomBytesFromRandomGenRef
    getRandomWordsFrom = getRandomWordsFromRandomGenRef

getRandomBytesFromStdGenIO :: Int -> IO [Word8]
getRandomBytesFromStdGenIO n = do
    ints <- replicateM n (randomRIO (0, 255))
    let bytes = map fromIntegral (ints :: [Int])
    return bytes

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
--
-- For example, if @x :: TVar StdGen@, @getRandomBytesFromRandomGenRef x@ can be
-- used as a 'RandomSource' in 'IO', 'STM', or any monad which is an instance
-- of 'MonadIO'.  It's generally probably better to use
-- 'getRandomWordsFromRandomGenRef' though, as this one is likely to throw
-- away a lot of perfectly good entropy.
getRandomBytesFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> Int -> m [Word8]
getRandomBytesFromRandomGenRef g n = do
    let swap (a,b) = (b,a)
    ints <- replicateM n (atomicModifyRef g (swap . randomR (0, 255)))
    let bytes = map fromIntegral (ints :: [Int])
    return bytes
    
-- |Similarly, @getRandomWordsFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
getRandomBytesFromRandomGenState :: (RandomGen g, MonadState g m) =>
                                  Int -> m [Word8]
getRandomBytesFromRandomGenState n = replicateM n $ do
    g <- get
    case randomR (0,255 :: Int) g of
        (i,g) -> do
            put g
            return (fromIntegral i)

-- |See 'getRandomBytesFromRandomGenRef'
getRandomWordsFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> Int -> m [Word64]
getRandomWordsFromRandomGenRef g n = do
    let swap (a,b) = (b,a)
    ints <- replicateM n (atomicModifyRef g (swap . randomR (0, 2^64-1)))
    let bytes = map fromInteger ints
    return bytes
    
-- |See 'getRandomBytesFromRandomGenState'
getRandomWordsFromRandomGenState :: (RandomGen g, MonadState g m) =>
                                  Int -> m [Word64]
getRandomWordsFromRandomGenState n = replicateM n $ do
    g <- get
    case randomR (0,2^64-1) g of
        (i,g) -> do
            put g
            return (fromInteger i)

instance MonadRandom (State StdGen) where
    getRandomBytes = getRandomBytesFromRandomGenState
    getRandomWords = getRandomWordsFromRandomGenState

instance Monad m => MonadRandom (StateT StdGen m) where
    getRandomBytes = getRandomBytesFromRandomGenState
    getRandomWords = getRandomWordsFromRandomGenState
