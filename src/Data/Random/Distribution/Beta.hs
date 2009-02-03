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
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Uniform

beta :: Distribution Beta a => a -> a -> RVar a
beta a b = sample (Beta a b)

data Beta a = Beta a a

instance (Fractional a, Distribution Gamma a, Distribution Uniform a) => Distribution Beta a where
    rvar (Beta 1 1) = uniform 0 1
    rvar (Beta alpha beta) = do
        x <- gamma alpha 1
        y <- gamma beta  1
        return (x / (x + y))
