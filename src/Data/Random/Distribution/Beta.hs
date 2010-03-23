{-
 -      ``Data/Random/Distribution/Beta''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Beta where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Uniform

fractionalBeta :: (Fractional a, Distribution Gamma a, Distribution StdUniform a) => a -> a -> RVar a
fractionalBeta 1 1 = stdUniform
fractionalBeta a b = do
    x <- gamma a 1
    y <- gamma b 1
    return (x / (x + y))

fractionalBetaFromIntegral :: (Fractional c, Distribution (Erlang a) c, Distribution (Erlang b) c) => a -> b -> RVar c
fractionalBetaFromIntegral a b =  do
    x <- erlang a
    y <- erlang b
    return (x / (x + y))

beta :: Distribution Beta a => a -> a -> RVar a
beta a b = rvar (Beta a b)

data Beta a = Beta a a

instance (Fractional a, Distribution Gamma a, Distribution StdUniform a) => Distribution Beta a where
    rvar (Beta a b) = fractionalBeta a b
