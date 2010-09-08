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

-- This dependency needs to be extracted into a releasable package or 
-- internal module before this module can be released.
import Math.Gamma

chiSquare :: Distribution ChiSquare t => Int -> RVar t
chiSquare = rvar . ChiSquare

chiSquareT :: Distribution ChiSquare t => Int -> RVarT m t
chiSquareT = rvar . ChiSquare

newtype ChiSquare b = ChiSquare Int

-- just brute force right now.  need better implementation (inverse incomplete
-- gamma func?).
instance (Num t, Distribution Normal t) => Distribution ChiSquare t where
    rvarT (ChiSquare n) = foldl' (\xs x -> xs + x^2) 0 `fmap` replicateM n stdNormalT

instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
    cdf (ChiSquare n) x = p (0.5 * fromIntegral n) (0.5 * realToFrac x)
