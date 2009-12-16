{-
 -      ``Data/Random/Distribution/Rayleigh''
 -}
{-# LANGUAGE
        MultiParamTypeClasses, 
        FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}

module Data.Random.Distribution.Rayleigh where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

floatingRayleigh :: (Floating a, Distribution StdUniform a) => a -> RVar a
floatingRayleigh s = do
    u <- stdUniformPos
    return (s * sqrt (-2 * log u))

newtype Rayleigh a = Rayleigh a

rayleigh :: Distribution Rayleigh a => a -> RVar a
rayleigh = rvar . Rayleigh

rayleighCDF :: Real a => a -> a -> Double
rayleighCDF s x = 1 - exp ((-0.5)* realToFrac (x*x) / realToFrac (s*s))

instance (RealFloat a, Distribution StdUniform a) => Distribution Rayleigh a where
    rvar (Rayleigh s) = floatingRayleigh s

instance (Real a, Distribution Rayleigh a) => CDF Rayleigh a where
    cdf  (Rayleigh s) x = rayleighCDF s x
