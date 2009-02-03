{-
 -      ``Data/Random/Distribution/Poisson''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, UndecidableInstances
  #-}

module Data.Random.Distribution.Poisson where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Binomial

-- from Knuth, with interpretation help from gsl sources
poisson :: (Distribution (Poisson b) a) => b -> RVar a
poisson mu = sample (Poisson mu)

data Poisson b a = Poisson b

instance (Distribution Uniform b, RealFloat b, Integral a) => Distribution (Poisson b) a where
    rvar (Poisson mu) = psn 0 mu
        where
            psn k mu
                | mu > 10   = do
                    let m = floor (mu * (7/8))
                
                    x <- erlang m 1
                    if x >= mu
                        then do
                            b <- binomial (m - 1) (mu / x)
                            return (k + b)
                        else psn (k + m) (mu - x)
                
                | otherwise = prod 1 k
                    where
                        emu = exp (-mu)
                    
                        prod p k = do
                            u <- uniform 0 1
                            if p * u > emu
                                then prod (p * u) (k + 1)
                                else return k

