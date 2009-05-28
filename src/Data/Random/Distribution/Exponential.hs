{-
 -      ``Data/Random/Distribution/Exponential''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Exponential where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

data Exponential a = Exp a

floatingExponential :: (Floating a, Distribution StdUniform a) => a -> RVar a
floatingExponential lambdaRecip = do
    x <- stdUniform
    return (negate (log x) * lambdaRecip)

floatingExponentialCDF :: Real a => a -> a -> Double
floatingExponentialCDF lambdaRecip x = 1 - exp (negate (realToFrac x) / realToFrac lambdaRecip)

exponential :: Distribution Exponential a => a -> RVar a
exponential = rvar . Exp

instance (Floating a, Distribution StdUniform a) => Distribution Exponential a where
    rvar (Exp lambdaRecip) = floatingExponential lambdaRecip
instance (Real a, Distribution Exponential a) => CDF Exponential a where
    cdf  (Exp lambdaRecip) = floatingExponentialCDF lambdaRecip