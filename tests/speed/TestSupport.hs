{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-type-defaults                  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures              #-}

module TestSupport where

import System.Random.Mersenne.Pure64
import System.Random.MWC
import Data.List
import Data.StateRef
import Foreign
import System.Random

-- type Src = IORef PureMT
-- getTestSource = newMTSrc

-- type Src = IORef StdGen
-- getTestSource = newStdSrc

type Src = Gen RealWorld
getTestSource :: IO Src
getTestSource = newGenIO

newMTSrc :: IO (IORef PureMT)
newMTSrc = do
    mt <- newPureMT
    newReference mt

newStdSrc :: IO (IORef StdGen)
newStdSrc = do
    mt <- newStdGen
    newReference mt

newGenIO :: IO (Gen RealWorld)
newGenIO = do
    seed <- withSystemRandom (save :: Gen RealWorld -> IO Seed)
    restore seed


sum' xs = foldl' (+) 0 xs

sumM m x = go m 0
    where
        go 0 s = return s
        go n s = do
            y <- x
            go (n - 1) $! (y + s)

sumBuf :: (Num t, Storable t) => Int -> Ptr t -> IO t
sumBuf bufSz ptr = go bufSz 0
    where
        go 0     s = return s
        go sz s = do
            x <- peekElemOff ptr sz
            go (sz - 1) $! (x + s)
