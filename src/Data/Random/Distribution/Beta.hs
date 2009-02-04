{-
 -      ``Data/Random/Distribution/Beta''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Beta where

import Data.Random.Source
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Uniform

import Control.Monad

realFloatBeta :: RealFloat a => a -> a -> RVar a
realFloatBeta 1 1 = realFloatStdUniform
realFloatBeta a b = do
    x <- realFloatGamma a 1
    y <- realFloatGamma b 1
    return (x / (x + y))

realFloatBetaFromIntegral :: (Integral a, Integral b, RealFloat c) => a -> b -> RVar c
realFloatBetaFromIntegral a b =  do
    x <- realFloatErlang a
    y <- realFloatErlang b
    return (x / (x + y))

beta :: Distribution Beta a => a -> a -> RVar a
beta a b = sample (Beta a b)

data Beta a = Beta a a

instance (RealFloat a) => Distribution Beta a where
    rvar (Beta a b) = realFloatBeta a b
