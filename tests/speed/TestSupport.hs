{-# LANGUAGE TypeFamilies #-}
module TestSupport where

import System.Random.Mersenne.Pure64
import System.Random.MWC
import Data.List
import Data.StateRef
import Control.Monad (forever)
import Control.Monad.ST
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

sumM n x = go n 0
    where
        go 0 s = return s
        go n s = do
            x <- x
            go (n - 1) $! (x + s)

sumBuf :: (Num t, Storable t) => Int -> Ptr t -> IO t
sumBuf bufSz ptr = go bufSz 0
    where
        go 0     s = return s
        go sz s = do
            x <- peekElemOff ptr sz
            go (sz - 1) $! (x + s)
