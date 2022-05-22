{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances, BangPatterns
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Gamma
    ( Gamma(..)
    , gamma, gammaT

    , Erlang(..)
    , erlang, erlangT

    , mtGamma
    ) where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal

import Data.Ratio

import Numeric.SpecFunctions

-- |derived from  Marsaglia & Tang, "A Simple Method for generating gamma
-- variables", ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372.
{-# SPECIALIZE mtGamma :: Functor m => Double -> Double -> RVarT m Double #-}
{-# SPECIALIZE mtGamma :: Functor m => Float  -> Float  -> RVarT m Float  #-}
mtGamma
    :: (Floating a, Ord a,
        Distribution StdUniform a,
        Distribution Normal a, Functor m)
    => a -> a -> RVarT m a
mtGamma a b
    | a < 1     = do
        u <- stdUniformT
        mtGamma (1+a) $! (b * u ** recip a)
    | otherwise = go
    where
        !d = a - fromRational (1%3)
        !c = recip (sqrt (9*d))

        go = do
            x <- stdNormalT
            let !v   = 1 + c*x

            if v <= 0
                then go
                else do
                    u  <- stdUniformT
                    let !x_2 = x*x; !x_4 = x_2*x_2
                        v3 = v*v*v
                        dv = d * v3
                    if      u < 1 - 0.0331*x_4
                     || log u < 0.5 * x_2 + d - dv + d*log v3
                        then return (b*dv)
                        else go

{-# SPECIALIZE gamma :: Float  -> Float  -> RVar Float  #-}
{-# SPECIALIZE gamma :: Double -> Double -> RVar Double #-}
gamma :: (Distribution Gamma a) => a -> a -> RVar a
gamma a b = rvar (Gamma a b)

gammaT :: (Distribution Gamma a, Functor m) => a -> a -> RVarT m a
gammaT a b = rvarT (Gamma a b)

erlang :: (Distribution (Erlang a) b) => a -> RVar b
erlang a = rvar (Erlang a)

erlangT :: (Distribution (Erlang a) b, Functor m) => a -> RVarT m b
erlangT a = rvarT (Erlang a)

data    Gamma a    = Gamma a a
newtype Erlang a b = Erlang a

instance (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a) => Distribution Gamma a where
    {-# SPECIALIZE instance Distribution Gamma Double #-}
    {-# SPECIALIZE instance Distribution Gamma Float #-}
    rvarT (Gamma a b) = mtGamma a b

instance (Real a, Distribution Gamma a) => CDF Gamma a where
    cdf (Gamma a b) x = incompleteGamma (realToFrac a) (realToFrac x / realToFrac b)

instance (Integral a, Floating b, Ord b, Distribution Normal b, Distribution StdUniform b) => Distribution (Erlang a) b where
    rvarT (Erlang a) = mtGamma (fromIntegral a) 1

instance (Integral a, Real b, Distribution (Erlang a) b) => CDF (Erlang a) b where
    cdf (Erlang a) x = incompleteGamma (fromIntegral a) (realToFrac x)
