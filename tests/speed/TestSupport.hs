{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-type-defaults                  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures             #-}

module TestSupport where

import System.Random.Mersenne.Pure64
import System.Random.MWC
import Data.List
import Foreign
import System.Random.Stateful
import Control.Monad.ST (RealWorld)

newMTGenM :: IO (IOGenM PureMT)
newMTGenM = newIOGenM =<< newPureMT

newStdGenM :: IO (IOGenM StdGen)
newStdGenM = newIOGenM =<< newStdGen

newMWCGenIO :: IO (Gen RealWorld)
newMWCGenIO = createSystemRandom


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
