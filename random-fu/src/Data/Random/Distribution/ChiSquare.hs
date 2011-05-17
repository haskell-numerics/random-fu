{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}
module Data.Random.Distribution.ChiSquare where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Control.Monad
import Data.List

import Math.Gamma (p)

chiSquare :: Distribution ChiSquare t => Int -> RVar t
chiSquare = rvar . ChiSquare

chiSquareT :: Distribution ChiSquare t => Int -> RVarT m t
chiSquareT = rvarT . ChiSquare

newtype ChiSquare b = ChiSquare Int

instance (Fractional t, Distribution Gamma t) => Distribution ChiSquare t where
    rvarT (ChiSquare n) = gammaT (0.5 * fromIntegral n) 2

instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
    cdf (ChiSquare n) x = p (0.5 * fromIntegral n) (0.5 * realToFrac x)
