{-
 -      ``Data/Random/Source''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , MonadRandomSeed(..)
    , RandomSource(..)
    , RandomSourceSeed(..)
    ) where

import Data.Word
import Data.Bits
import Data.List

import Data.Random.Internal.Words

class MonadRandom m => MonadRandomSeed m where
    setRandomState :: RandomSource m seed => seed -> m ()

class Monad m => MonadRandom m where
    -- |get the specified number of random (uniformly distributed) bytes
    getRandomBytes :: Int -> m [Word8]
    getRandomBytes n
        | n .&. 7 == 0
        = do
            let wc = n `shiftR` 3
            ws <- getRandomWords wc
            return (concatMap wordToBytes ws)
        | otherwise
        = do
            let wc = (n `shiftR` 3) + 1
            ws <- getRandomWords wc
            return . take n . concatMap wordToBytes $ ws
        
    -- |alternate basis function, providing access to larger chunks
    getRandomWords :: Int -> m [Word64]
    getRandomWords n = do
        bs <- getRandomBytes (n `shiftL` 3)
        return (bytesToWords bs)

class Monad m => RandomSource m s where
    getRandomBytesFrom :: s -> Int -> m [Word8]
    getRandomBytesFrom src n
        | n .&. 7 == 0
        = do
            let wc = n `shiftR` 3
            ws <- getRandomWordsFrom src wc
            return (concatMap wordToBytes ws)
        | otherwise
        = do
            let wc = (n `shiftR` 3) + 1
            ws <- getRandomWordsFrom src wc
            return . take n . concatMap wordToBytes $ ws
        
    
    getRandomWordsFrom :: s -> Int -> m [Word64]
    getRandomWordsFrom src n = do
        bs <- getRandomBytesFrom src (n `shiftL` 3)
        return (bytesToWords bs)

class RandomSource m s => RandomSourceSeed m s where
    setRandomStateFor :: RandomSource m seed => s -> seed -> m ()

instance Monad m => RandomSource m (Int -> m [Word8]) where
    getRandomBytesFrom = id

instance Monad m => RandomSource m (Int -> m [Word64]) where
    getRandomWordsFrom = id

