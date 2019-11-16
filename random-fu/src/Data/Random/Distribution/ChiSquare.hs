{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}
module Data.Random.Distribution.ChiSquare where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma

import Numeric.SpecFunctions

chiSquare :: Distribution ChiSquare t => Integer -> RVar t
chiSquare = rvar . ChiSquare

chiSquareT :: Distribution ChiSquare t => Integer -> RVarT m t
chiSquareT = rvarT . ChiSquare

newtype ChiSquare b = ChiSquare Integer

instance (Fractional t, Distribution Gamma t) => Distribution ChiSquare t where
    rvarT (ChiSquare 0) = return 0
    rvarT (ChiSquare n)
        | n > 0     = gammaT (0.5 * fromInteger n) 2
        | otherwise = error "chi-square distribution: degrees of freedom must be positive"

instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
    cdf (ChiSquare n) x = incompleteGamma (0.5 * fromInteger n) (0.5 * realToFrac x)
