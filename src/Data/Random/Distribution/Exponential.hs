{-
 -      ``Data/Random/Distribution/Exponential''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Exponential where

import Data.Random.Source
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

data Exponential a = Exp a

floatingExponential :: (Floating a, Distribution StdUniform a) => a -> RVar a
floatingExponential lambdaRecip = do
    x <- stdUniform
    return (negate (log x) * lambdaRecip)

exponential :: Distribution Exponential a => a -> RVar a
exponential = rvar . Exp

instance (Floating a, Distribution StdUniform a) => Distribution Exponential a where
    rvar (Exp lambdaRecip) = floatingExponential lambdaRecip
