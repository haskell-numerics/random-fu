{-
 -      ``Data/Random/Distribution''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution where

import Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.Std
import Data.Word

-- |A definition of a random variable's distribution.  From the distribution
-- an 'RVar' can be created, or the distribution can be directly sampled.
-- 'RVar' in particular is an instance of 'Distribution', and so can be 'sample'd.
--
-- Minimum instance definition: 'rvarT'.
class Distribution d t where
    -- |Return a random variable with this distribution.
    rvarT :: d t -> RVarT n t

rvar :: (Distribution d t) => d t -> RVar t
rvar = rvarT
