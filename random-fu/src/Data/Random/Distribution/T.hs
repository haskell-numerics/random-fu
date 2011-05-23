{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Random.Distribution.T where

import Data.RVar
import Data.Random.Distribution
import Data.Random.Distribution.ChiSquare
import Data.Random.Distribution.Normal
import Math.Beta

t :: Distribution T a => Integer -> RVar a
t = rvar . T

tT :: Distribution T a => Integer -> RVarT m a
tT = rvarT . T

newtype T a = T Integer
    deriving (Eq, Ord, Show)

instance (Floating a, Distribution Normal a, Distribution ChiSquare a) => Distribution T a where
    rvarT (T n)
        | n > 0     = do
            x <- stdNormalT
            y <- chiSquareT n
            return (x * sqrt (fromInteger n / y))
        | otherwise = fail "Student's t-distribution: degrees of freedom must be positive"

instance (Real a, Distribution T a) => CDF T a where
    cdf (T n) t = i_ x v2 v2
        where
            v = fromIntegral n
            v2 = 0.5 * v
            tD = realToFrac t
            u = sqrt (tD*tD + v)
            x = (tD + u) / (u + u)