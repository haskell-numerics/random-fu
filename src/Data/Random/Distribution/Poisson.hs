{-
 -      ``Data/Random/Distribution/Poisson''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, UndecidableInstances,
    TemplateHaskell
  #-}

module Data.Random.Distribution.Poisson where

import Data.Random.Internal.TH

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Binomial

import Control.Monad

-- from Knuth, with interpretation help from gsl sources
integralPoisson :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => b -> RVar a
integralPoisson = psn 0
    where
        psn :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => a -> b -> RVar a
        psn j mu
            | mu > 10   = do
                let m = floor (mu * (7/8))
            
                x <- erlang m
                if x >= mu
                    then do
                        b <- binomial (m - 1) (mu / x)
                        return (j + b)
                    else psn (j + m) (mu - x)
            
            | otherwise = prod 1 j
                where
                    emu = exp (-mu)
                
                    prod p k = do
                        u <- stdUniform
                        if p * u > emu
                            then prod (p * u) (k + 1)
                            else return k

integralPoissonCDF :: (Integral a, Real b) => b -> a -> Double
integralPoissonCDF mu k = exp (negate lambda) * sum
    [ lambda ^^ i / i_fac
    | (i, i_fac) <- zip [0..k] (scanl (*) 1 [1..])
    ]
    
    where lambda = realToFrac mu

fractionalPoisson :: (Num a, Distribution (Poisson b) Integer) => b -> RVar a
fractionalPoisson mu = liftM fromInteger (poisson mu)

fractionalPoissonCDF :: (CDF (Poisson b) Integer, RealFrac a) => b -> a -> Double
fractionalPoissonCDF mu k = cdf (Poisson mu) (floor k :: Integer)

poisson :: (Distribution (Poisson b) a) => b -> RVar a
poisson mu = rvar (Poisson mu)

data Poisson b a = Poisson b

$( replicateInstances ''Int integralTypes [d|
        instance ( RealFloat b 
                 , Distribution StdUniform   b
                 , Distribution (Erlang Int) b
                 , Distribution (Binomial b) Int
                 ) => Distribution (Poisson b) Int where
            rvar (Poisson mu) = integralPoisson mu
        instance (Real b, Distribution (Poisson b) Int) => CDF (Poisson b) Int where
            cdf  (Poisson mu) = integralPoissonCDF mu
    |] )

$( replicateInstances ''Float realFloatTypes [d|
        instance (Distribution (Poisson b) Integer) => Distribution (Poisson b) Float where
            rvar (Poisson mu) = fractionalPoisson mu
        instance (CDF (Poisson b) Integer) => CDF (Poisson b) Float where
            cdf  (Poisson mu) = fractionalPoissonCDF mu
    |])
