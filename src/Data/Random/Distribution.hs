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
    sampleFrom :: RandomSource m s => s -> d t -> m t

sample :: (Distribution d t, MonadRandom m) => d t -> m t
sample = (sampleFrom :: (Distribution d t, MonadRandom m) => (Int -> m [Word8]) -> d t -> m t) getRandomBytes

sampleUntil :: Monad m => (a -> Bool) -> m a -> m a
sampleUntil p d = do
    x <- d
    if p x then return x
        else sampleUntil p d
