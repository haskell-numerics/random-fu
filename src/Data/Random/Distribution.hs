{-
 -      ``Data/Random/Distribution''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution where

import {-# SOURCE #-} Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std
import Data.Word

class Distribution d t where
    rvar :: d t -> RVar t
    rvar = sampleFrom StdRandom
    sampleFrom :: RandomSource m s => s -> d t -> m t
    sampleFrom src dist = sampleFrom src (rvar dist)

sample :: (Distribution d t, MonadRandom m) => d t -> m t
sample = (sampleFrom :: (Distribution d t, MonadRandom m) => (Int -> m [Word8]) -> d t -> m t) getRandomBytes
