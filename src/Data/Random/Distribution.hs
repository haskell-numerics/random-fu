{-
 -      ``Data/Random/Distribution''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution where

import {-# SOURCE #-} Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std
import Data.Word

-- |A definition of a random variable's distribution.  From the distribution
-- an 'RVar' can be created, or the distribution can be directly sampled.
-- 'RVar' in particular is an instance of 'Distribution', and so can be 'sample'd.
--
-- Minimum instance definition: either 'rvar' or 'sampleFrom'.
class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: d t -> RVar t
    rvar = sampleFrom StdRandom
    
    -- |Directly sample from the distribution, given a source of entropy.
    sampleFrom :: RandomSource m s => s -> d t -> m t
    sampleFrom src dist = sampleFrom src (rvar dist)

-- |Sample a distribution using the default source of entropy for the
-- monad in which the sampling occurs.
sample :: (Distribution d t, MonadRandom m) => d t -> m t
sample = sampleFrom StdRandom
