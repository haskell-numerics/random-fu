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
import NR.Ch6.S2

chiSquare :: Distribution ChiSquare t => Int -> RVar t
chiSquare = rvar . ChiSquare

newtype ChiSquare b = ChiSquare Int

-- just brute force right now.  need better implementation (inverse incomplete
-- gamma func?).
instance (Num t, Distribution Normal t) => Distribution ChiSquare t where
    rvar (ChiSquare n) = foldl' (\xs x -> xs + x^2) 0 `fmap` replicateM n stdNormal

instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
    cdf (ChiSquare n) x = gammp (0.5 * fromIntegral n) (0.5 * realToFrac x)