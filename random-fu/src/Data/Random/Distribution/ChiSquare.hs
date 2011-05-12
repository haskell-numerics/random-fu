{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}
module Data.Random.Distribution.ChiSquare where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Normal
import Control.Monad
import Data.List

import Math.Gamma

chiSquare :: Distribution ChiSquare t => Int -> RVar t
chiSquare = rvar . ChiSquare

chiSquareT :: Distribution ChiSquare t => Int -> RVarT m t
chiSquareT = rvarT . ChiSquare

newtype ChiSquare b = ChiSquare Int

-- just brute force right now.  need better implementation (inverse incomplete
-- gamma func?).
instance (Num t, Distribution Normal t) => Distribution ChiSquare t where
    rvarT (ChiSquare n) = sumSquares `fmap` replicateM n stdNormalT
        where sumSquares = foldl' (\xs x -> xs + x^2) 0 

instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
    cdf (ChiSquare n) x = p (0.5 * fromIntegral n) (0.5 * realToFrac x)
