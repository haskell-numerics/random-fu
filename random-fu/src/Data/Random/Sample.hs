{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts,
        IncoherentInstances
  #-}

module Data.Random.Sample where

import Control.Monad.State
import Data.Random.Distribution
import Data.Random.Lift
import Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std

-- |A typeclass allowing 'Distribution's and 'RVar's to be sampled.  Both may
-- also be sampled via 'runRVar' or 'runRVarT', but I find it psychologically
-- pleasing to be able to sample both using this function, as they are two
-- separate abstractions for one base concept: a random variable.
class Sampleable d m t where
    -- |Directly sample from a distribution or random variable, using the given source of entropy.
    sampleFrom :: RandomSource m s => s -> d t -> m t

instance Distribution d t => Sampleable d m t where
    sampleFrom src d = runRVarT (rvar d) src

-- This instance overlaps with the other, but because RVarT is not a Distribution there is no conflict.
instance Lift m n => Sampleable (RVarT m) n t where
    sampleFrom src x = runRVarT x src

-- |Sample a random variable using the default source of entropy for the
-- monad in which the sampling occurs.
sample :: (Sampleable d m t, MonadRandom m) => d t -> m t
sample = sampleFrom StdRandom

-- |Sample a random variable in a \"functional\" style.  Typical instantiations
-- of @s@ are @System.Random.StdGen@ or @System.Random.Mersenne.Pure64.PureMT@.
sampleState :: (RandomGen s, Sampleable d (State s) t, MonadRandom (State s)) => d t -> s -> (t, s)
sampleState thing = runState (sample thing)

-- |Sample a random variable in a \"semi-functional\" style.  Typical instantiations
-- of @s@ are @System.Random.StdGen@ or @System.Random.Mersenne.Pure64.PureMT@.
sampleStateT :: (RandomGen s, Sampleable d (StateT s m) t, MonadRandom (StateT s m)) => d t -> s -> m (t, s)
sampleStateT thing = runStateT (sample thing)
