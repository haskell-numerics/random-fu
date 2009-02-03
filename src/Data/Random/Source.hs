{-
 -      ``Data/Random/Source''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , MonadRandomSeed(..)
    , RandomSource(..)
    , RandomSourceSeed(..)
    
    , getNByteInteger
    , getNByteIntegerWhere
    , getNBitInteger
    ) where

import Data.Word
import Data.Bits

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

getNByteInteger :: RandomSource m s => s -> Int -> m Integer
getNByteInteger src n = do
    xs <- getRandomBytesFrom src n
    return (concatBytes xs)

getNByteIntegerWhere :: (RandomSource m s) => s -> Int -> (Integer -> Bool) -> m Integer
getNByteIntegerWhere src n p = do
    initialBytes <- getRandomBytesFrom src n
    let mask = 256 ^ n - 1
    
    let compute x
            | p x       = return x
            | otherwise = do
                [newByte] <- getRandomBytesFrom src 1
                compute (mask .&. (x `shiftL` 8) .|. toInteger newByte)
        
    
    compute (concatBytes initialBytes)

getNBitInteger :: RandomSource m s => s -> Int -> m Integer
getNBitInteger src n
    | n .&. 7 == 0
    = getNByteInteger src (n `shiftR` 3)
    | otherwise
    = do
    x <- getNByteInteger src ((n `shiftR` 3) + 1)
    return (x .&. (bit n - 1))

concatBytes :: (Bits a, Num a) => [Word8] -> a
concatBytes = concatBits fromIntegral

concatBits :: (Bits a, Bits b, Num b) => (a -> b) -> [a] -> b
concatBits f [] = 0
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)

