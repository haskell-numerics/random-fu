{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.StretchedExponential where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

newtype StretchedExponential a = StretchedExp (a,a)

floatingStretchedExponential :: (Floating a, Distribution StdUniform a) => a -> a -> RVarT m a
floatingStretchedExponential beta lambdaRecip = do
    x <- stdUniformT
    return (negate (log (1-x))**(1/beta) * lambdaRecip)

floatingStretchedExponentialCDF :: Real a => a -> a -> a -> Double
floatingStretchedExponentialCDF beta lambdaRecip x = 1 - exp (negate (realToFrac x / realToFrac lambdaRecip)**(realToFrac beta))

stretchedExponential :: Distribution StretchedExponential a => a -> a -> RVar a
stretchedExponential beta lambdaRecip = rvar $ StretchedExp (beta, lambdaRecip)

stretchedExponentialT :: Distribution StretchedExponential a => a -> a -> RVarT m a
stretchedExponentialT beta lambdaRecip = rvarT $ StretchedExp (beta, lambdaRecip)

instance (Floating a, Distribution StdUniform a) => Distribution StretchedExponential a where
    rvarT (StretchedExp (beta,lambdaRecip)) = floatingStretchedExponential beta lambdaRecip
instance (Real a, Distribution StretchedExponential a) => CDF StretchedExponential a where
    cdf  (StretchedExp (beta,lambdaRecip)) = floatingStretchedExponentialCDF beta lambdaRecip