{-
 -      ``Data/Random/Source''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source where

import Data.Word

class MonadRandom m => MonadRandomSeed m where
    getRandomState :: m [Word8]
    setRandomState :: (Int -> m [Word8]) -> m ()

class Monad m => MonadRandom m where
    -- |get the specified number of random (uniformly distributed) bytes
    getRandomBytes :: Int -> m [Word8]

class Monad m => RandomSource m s where
    getRandomBytesFrom :: s -> Int -> m [Word8]

class RandomSource m s => RandomSourceSeed m s where
    getRandomStateFor :: s -> m [Word8]
    setRandomStateFor :: s -> (Int -> m [Word8]) -> m ()

instance Monad m => RandomSource m (Int -> m [Word8]) where
    getRandomBytesFrom = id
