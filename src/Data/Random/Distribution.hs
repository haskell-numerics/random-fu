{-
 -      ``Data/Random/Distribution''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Data.Random.Distribution where

import Data.Random.Source
import Data.Word

class Distribution d t where
    getDistFrom :: RandomSource m s => s -> d t -> m t

getDist :: (Distribution d t, MonadRandom m) => d t -> m t
getDist = (getDistFrom :: (Distribution d t, MonadRandom m) => (Int -> m [Word8]) -> d t -> m t) getRandomBytes
