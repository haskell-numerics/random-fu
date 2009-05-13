{-
 -      ``Data/Random/Distribution''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution where

import Data.Random.Lift
import Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std
import Data.Word

-- |A definition of a random variable's distribution.  From the distribution
-- an 'RVar' can be created, or the distribution can be directly sampled using 
-- 'sampleFrom' or 'sample'.
class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: (Distribution d t) => d t -> RVar t
    rvar = rvarT

-- |Return a random variable with the given distribution, pre-lifted to an arbitrary 'RVarT'.
-- Any arbitrary 'RVar' can also be converted to an 'RVarT m' for an arbitrary 'm', using
-- either 'lift' or 'sample'.
rvarT :: Distribution d t => d t -> RVarT n t
rvarT d = lift (rvar d)

