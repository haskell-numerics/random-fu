{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts,
        IncoherentInstances
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Sample where

import Control.Monad.State
import Control.Monad.Reader
import Data.Random.Distribution
import Data.Random.Lift
import Data.Random.RVar

import System.Random.Stateful

-- |A typeclass allowing 'Distribution's and 'RVar's to be sampled.  Both may
-- also be sampled via 'runRVar' or 'runRVarT', but I find it psychologically
-- pleasing to be able to sample both using this function, as they are two
-- separate abstractions for one base concept: a random variable.
class Sampleable d m t where
    -- |Directly sample from a distribution or random variable, using the given source of entropy.
    sampleFrom :: StatefulGen g m => g -> d t -> m t

instance Distribution d t => Sampleable d m t where
    sampleFrom gen d = runRVarT (rvar d) gen

-- This instance overlaps with the other, but because RVarT is not a Distribution there is no conflict.
instance Lift m n => Sampleable (RVarT m) n t where
    sampleFrom gen x = runRVarT x gen

-- |Sample a random variable using the default source of entropy for the
-- monad in which the sampling occurs.
sample :: (Sampleable d m t, StatefulGen g m, MonadReader g m) => d t -> m t
sample thing = ask >>= \gen -> sampleFrom gen thing

-- |Sample a random variable in a \"functional\" style.  Typical instantiations
-- of @s@ are @System.Random.StdGen@ or @System.Random.Mersenne.Pure64.PureMT@.
-- sample :: (Distribution d a, StatefulGen g m, MonadReader g m) => d t -> m t
-- sample thing gen = runStateGen gen (\stateGen -> sampleFrom stateGen thing)

sampleState :: (Distribution d t, RandomGen g, MonadState g m) => d t -> m t
sampleState thing = sampleFrom StateGenM thing

-- |Sample a random variable in a \"functional\" style.  Typical instantiations
-- of @g@ are @System.Random.StdGen@ or @System.Random.Mersenne.Pure64.PureMT@.
samplePure :: (Distribution d t, RandomGen g) => d t -> g -> (t, g)
samplePure thing gen = runStateGen gen (\stateGen -> sampleFrom stateGen thing)
