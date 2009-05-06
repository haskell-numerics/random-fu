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

realFloatExponential :: RealFloat a => a -> RVarT m a
realFloatExponential lambdaRecip = do
    x <- realFloatStdUniform
    return (negate (log x) * lambdaRecip)

exponential :: Distribution Exponential a => a -> RVarT m a
exponential = rvarT . Exp

instance (RealFloat a) => Distribution Exponential a where
    rvarT (Exp lambdaRecip) = realFloatExponential lambdaRecip
