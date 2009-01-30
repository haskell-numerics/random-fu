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
    getDistFrom src (Beta 1 1) = 
        getDistFrom src (Uniform 0 1)
        
    getDistFrom src (Beta alpha beta) = do
        x <- getDistFrom src (Gamma alpha 1)
        y <- getDistFrom src (Gamma beta  1)
        return (x / (x + y))
