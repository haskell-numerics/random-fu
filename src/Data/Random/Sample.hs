{-
 -      ``Data/Random/Sample''
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, 
        IncoherentInstances
  #-}

module Data.Random.Sample where

import Data.Random.Distribution
import Data.Random.Lift
import Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std

-- |A typeclass allowing 'Distribution's and 'RVar's to be sampled.  Both may
-- also be sampled via 'runRVar' or 'runRVarT', but I find it psychologically
-- pleasing to be able to sample both using this function.
class Sampleable d m t where
    -- |Directly sample from a distribution or random variable, using the given source of entropy.
    sampleFrom :: RandomSource m s => s -> d t -> m t

instance Distribution d t => Sampleable d m t where
    sampleFrom src d = runRVarT (rvar d) src

-- This instance overlaps with the other, but because RVarT is not a Distribution there is no conflict.
instance Lift m n => Sampleable (RVarT m) n t where
    sampleFrom src x = runRVarT x src

-- |Sample a distribution using the default source of entropy for the
-- monad in which the sampling occurs.
sample :: (Sampleable d m t, MonadRandom m) => d t -> m t
sample = sampleFrom StdRandom
