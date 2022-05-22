{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Exponential where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

{-|
A definition of the exponential distribution over the type @a@.

@'Exp' mu@ models an exponential distribution with mean @mu@. This can
alternatively be viewed as an exponential distribution with parameter @lambda =
1 / mu@.

See also 'exponential'.
-}
newtype Exponential a = Exp a

floatingExponential :: (Floating a, Distribution StdUniform a, Functor m) => a -> RVarT m a
floatingExponential lambdaRecip = do
    x <- stdUniformT
    return (negate (log x) * lambdaRecip)

floatingExponentialCDF :: Real a => a -> a -> Double
floatingExponentialCDF lambdaRecip x = 1 - exp (negate (realToFrac x) / realToFrac lambdaRecip)

{-|
A random variable which samples from the exponential distribution.

@'exponential' mu@ is an exponential random variable with mean @mu@. This can
alternatively be viewed as an exponential random variable with parameter @lambda
= 1 / mu@.
-}
exponential :: Distribution Exponential a => a -> RVar a
exponential = rvar . Exp

{-|
A random variable transformer which samples from the exponential distribution.

@'exponentialT' mu@ is an exponential random variable with mean @mu@. This can
alternatively be viewed as an exponential random variable with parameter @lambda
= 1 / mu@.
-}
exponentialT :: (Distribution Exponential a, Functor m) => a -> RVarT m a
exponentialT = rvarT . Exp

instance (Floating a, Distribution StdUniform a) => Distribution Exponential a where
    rvarT (Exp lambdaRecip) = floatingExponential lambdaRecip
instance (Real a, Distribution Exponential a) => CDF Exponential a where
    cdf  (Exp lambdaRecip) = floatingExponentialCDF lambdaRecip
