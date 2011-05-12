{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Random.Source.IO () where

import Data.Random.Internal.Source

#ifndef windows

import Data.Random.Source.DevRandom
instance MonadRandom IO where
    getRandomPrim = getRandomPrimFrom DevURandom

#else

import Data.Random.Source.MWC
import System.Random.MWC

instance MonadRandom IO where
    getRandomPrim = withSystemRandom . (flip getRandomPrimFrom :: Prim t -> Gen RealWorld -> IO t)

#endif