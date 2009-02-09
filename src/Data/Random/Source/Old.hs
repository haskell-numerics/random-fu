{-
 -      ``Data/Random/Source/Old''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances
  #-}

module Data.Random.Source.Old where

import Data.Random.Source
import System.Random
import Control.Monad
import Control.Monad.State
import Data.StateRef
import Data.Word

instance (ModifyRef (IORef   StdGen) m StdGen) => RandomSource m (IORef   StdGen) where getRandomBytesFrom = getRandomBytesFromRandomGenRef
instance (ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where getRandomBytesFrom = getRandomBytesFromRandomGenRef
instance (ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where getRandomBytesFrom = getRandomBytesFromRandomGenRef

getRandomBytesFromStdGenIO :: Int -> IO [Word8]
getRandomBytesFromStdGenIO n = do
    ints <- replicateM n (randomRIO (0, 255))
    let bytes = map fromIntegral (ints :: [Int])
    return bytes

getRandomBytesFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> Int -> m [Word8]
getRandomBytesFromRandomGenRef g n = do
    let swap (a,b) = (b,a)
    ints <- replicateM n (atomicModifyRef g (swap . randomR (0, 255)))
    let bytes = map fromIntegral (ints :: [Int])
    return bytes
    
getRandomBytesFromRandomGenState :: (RandomGen g, MonadState g m) =>
                                  Int -> m [Word8]
getRandomBytesFromRandomGenState n = replicateM n $ do
    g <- get
    case randomR (0,255 :: Int) g of
        (i,g) -> do
            put g
            return (fromIntegral i)

getRandomWordsFromRandomGenRef :: (ModifyRef sr m g, RandomGen g) =>
                                  sr -> Int -> m [Word64]
getRandomWordsFromRandomGenRef g n = do
    let swap (a,b) = (b,a)
    ints <- replicateM n (atomicModifyRef g (swap . randomR (0, 2^64-1)))
    let bytes = map fromInteger ints
    return bytes
    
getRandomWordsFromRandomGenState :: (RandomGen g, MonadState g m) =>
                                  Int -> m [Word64]
getRandomWordsFromRandomGenState n = replicateM n $ do
    g <- get
    case randomR (0,2^64-1) g of
        (i,g) -> do
            put g
            return (fromInteger i)

data RandomGenState g = RandomGenState

instance (MonadState g m, RandomGen g) => RandomSource m (RandomGenState g) where
    getRandomBytesFrom _ = getRandomBytesFromRandomGenState
    getRandomWordsFrom _ = getRandomWordsFromRandomGenState

instance MonadRandom (State StdGen) where
    getRandomBytes = getRandomBytesFromRandomGenState
    getRandomWords = getRandomWordsFromRandomGenState

instance Monad m => MonadRandom (StateT StdGen m) where
    getRandomBytes = getRandomBytesFromRandomGenState
    getRandomWords = getRandomWordsFromRandomGenState
