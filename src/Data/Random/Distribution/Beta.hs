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

{-# SPECIALIZE fractionalBeta :: Float  -> Float  -> RVar Float #-}
{-# SPECIALIZE fractionalBeta :: Double -> Double -> RVar Double #-}
fractionalBeta :: (Fractional a, Distribution Gamma a, Distribution StdUniform a) => a -> a -> RVar a
fractionalBeta 1 1 = stdUniform
fractionalBeta a b = do
    x <- gamma a 1
    y <- gamma b 1
    return (x / (x + y))

{-# SPECIALIZE beta :: Float  -> Float  -> RVar Float #-}
{-# SPECIALIZE beta :: Double -> Double -> RVar Double #-}
beta :: Distribution Beta a => a -> a -> RVar a
beta a b = rvar (Beta a b)

data Beta a = Beta a a

instance (Fractional a, Distribution Gamma a, Distribution StdUniform a) => Distribution Beta a where
    rvar (Beta a b) = fractionalBeta a b
