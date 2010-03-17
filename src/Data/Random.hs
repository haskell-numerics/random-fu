{-
 -      ``Data/Random''
 -}
{-# LANGUAGE
    FlexibleContexts
  #-}

-- |Random numbers and stuff...
-- 
-- "Data.Random.Source" exports the typeclasses for entropy sources, and
-- Data.Random.Source.* export various instances and/or functions with which
-- instances can be defined.
-- 
-- "Data.Random.Distribution" exports the typeclasses for sampling distributions,
-- and Data.Random.Distribution.* export various specific distributions.
--
-- "Data.Random.RVar" exports the 'RVar' type, which is a probability distribution
-- monad that allows for concise definitions of random variables, as well as
-- a couple handy 'RVar's.

module Data.Random
    ( module Data.Random.Sample
    , module Data.Random.Source
    , module Data.Random.Source.DevRandom
    , module Data.Random.Source.StdGen
    , module Data.Random.Source.PureMT
    , module Data.Random.Source.Std
    , module Data.Random.Distribution
    , module Data.Random.Distribution.Bernoulli
    , module Data.Random.Distribution.Beta
    , module Data.Random.Distribution.Binomial
    , module Data.Random.Distribution.Categorical
    , module Data.Random.Distribution.Gamma
    , module Data.Random.Distribution.Exponential
    , module Data.Random.Distribution.Normal
    , module Data.Random.Distribution.Poisson
    , module Data.Random.Distribution.Rayleigh
    , module Data.Random.Distribution.Triangular
    , module Data.Random.Distribution.Uniform
    , module Data.Random.Distribution.Ziggurat
    , module Data.Random.List
    , module Data.Random.RVar
    ) where

import Data.Random.Sample
import Data.Random.Source
import Data.Random.Source.DevRandom
import Data.Random.Source.MWC ()
import Data.Random.Source.StdGen
import Data.Random.Source.PureMT
import Data.Random.Source.Std
import Data.Random.Distribution
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Categorical
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Rayleigh
import Data.Random.Distribution.Triangular
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Ziggurat
import Data.Random.Lift ()
import Data.Random.List
import Data.Random.RVar

