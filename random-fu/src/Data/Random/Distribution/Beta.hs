{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances,
    TemplateHaskell
  #-}

module Data.Random.Distribution.Beta where

import Data.Random.Internal.TH

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Uniform

import Numeric.SpecFunctions

{-# SPECIALIZE fractionalBeta :: Float  -> Float  -> RVarT m Float #-}
{-# SPECIALIZE fractionalBeta :: Double -> Double -> RVarT m Double #-}
fractionalBeta :: (Fractional a, Eq a, Distribution Gamma a, Distribution StdUniform a) => a -> a -> RVarT m a
fractionalBeta 1 1 = stdUniformT
fractionalBeta a b = do
    x <- gammaT a 1
    y <- gammaT b 1
    return (x / (x + y))

{-# SPECIALIZE beta :: Float  -> Float  -> RVar Float #-}
{-# SPECIALIZE beta :: Double -> Double -> RVar Double #-}
beta :: Distribution Beta a => a -> a -> RVar a
beta a b = rvar (Beta a b)

{-# SPECIALIZE betaT :: Float  -> Float  -> RVarT m Float #-}
{-# SPECIALIZE betaT :: Double -> Double -> RVarT m Double #-}
betaT :: Distribution Beta a => a -> a -> RVarT m a
betaT a b = rvarT (Beta a b)

data Beta a = Beta a a

-- FIXME: I am far from convinced that NaNs are a good idea.
logBetaPdf :: Double -> Double -> Double -> Double
logBetaPdf a b x
   | a <= 0 || b <= 0 = nan
   | x <= 0 = log 0
   | x >= 1 = log 0
   | otherwise = (a-1)*log x + (b-1)*log (1-x) - logBeta a b
  where
    nan = 0.0 / 0.0

instance PDF Beta Double
  where
    pdf (Beta a b) = exp . logBetaPdf a b

instance PDF Beta Float
  where
    pdf (Beta a b) = realToFrac . exp . logBetaPdf (realToFrac a) (realToFrac b) . realToFrac

$( replicateInstances ''Float realFloatTypes [d|
        instance Distribution Beta Float
              where rvarT (Beta a b) = fractionalBeta a b
    |])
