{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |For convenience, this module defines an instance of 'MonadRandom' for the 'IO' monad.
-- On Windows it uses "Data.Random.Source.MWC" and on other platforms it uses
-- "Data.Random.Source.DevRandom".
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