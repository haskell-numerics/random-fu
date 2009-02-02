{-
 -      ``Data/Random/Distribution/Beta''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Beta where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Uniform

data Beta a = Beta a a

instance (Fractional a, Distribution Gamma a, Distribution Uniform a) => Distribution Beta a where
    sampleFrom src (Beta 1 1) = 
        sampleFrom src (Uniform 0 1)
        
    sampleFrom src (Beta alpha beta) = do
        x <- sampleFrom src (Gamma alpha 1)
        y <- sampleFrom src (Gamma beta  1)
        return (x / (x + y))
