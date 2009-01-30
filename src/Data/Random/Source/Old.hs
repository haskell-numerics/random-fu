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

