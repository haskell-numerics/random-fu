{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Random.Source.IO () where

import Data.Random.Internal.Source

#ifndef windows

import Data.Random.Source.DevRandom
instance MonadRandom IO where
    getRandomPrim = getRandomPrimFrom DevURandom

#else

import Control.Concurrent.MVar
import Control.Monad.Primitive (RealWorld)
import Data.Random.Source.MWC
import System.Random.MWC
import System.IO.Unsafe

-- Is MWC already threadsafe?  I don't know.
{-# NOINLINE globalMWC #-}
globalMWC :: MVar (Gen RealWorld)
globalMWC = unsafePerformIO (create >>= newMVar)

instance MonadRandom IO where
    getRandomPrim p = withMVar globalMWC (flip getRandomPrimFrom p)

#endif