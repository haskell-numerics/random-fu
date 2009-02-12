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

realFloatExponential :: RealFloat a => a -> RVar a
realFloatExponential lambdaRecip = do
    x <- realFloatStdUniform
    return (negate (log x) * lambdaRecip)

exponential :: Distribution Exponential a => a -> RVar a
exponential = sample . Exp

instance (RealFloat a) => Distribution Exponential a where
    rvar (Exp lambdaRecip) = realFloatExponential lambdaRecip
