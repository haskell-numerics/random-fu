{-# LANGUAGE
        MultiParamTypeClasses, 
        FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}

module Data.Random.Distribution.Rayleigh where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

floatingRayleigh :: (Floating a, Distribution StdUniform a) => a -> RVarT m a
floatingRayleigh s = do
    u <- stdUniformPosT
    return (s * sqrt (-2 * log u))

-- |The rayleigh distribution with a specified mode (\"sigma\") parameter.
-- Its mean will be @sigma*sqrt(pi/2)@ and its variance will be @sigma^2*(4-pi)/2@
-- 
-- (therefore if you want one with a particular mean @m@, @sigma@ should be @m*sqrt(2/pi)@)
newtype Rayleigh a = Rayleigh a

rayleigh :: Distribution Rayleigh a => a -> RVar a
rayleigh = rvar . Rayleigh

rayleighT :: Distribution Rayleigh a => a -> RVarT m a
rayleighT = rvarT . Rayleigh

rayleighCDF :: Real a => a -> a -> Double
rayleighCDF s x = 1 - exp ((-0.5)* realToFrac (x*x) / realToFrac (s*s))

instance (RealFloat a, Distribution StdUniform a) => Distribution Rayleigh a where
    rvarT (Rayleigh s) = floatingRayleigh s

instance (Real a, Distribution Rayleigh a) => CDF Rayleigh a where
    cdf  (Rayleigh s) x = rayleighCDF s x
