{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module Data.Random.Distribution.T where

import Data.RVar
import Data.Random.Distribution
import Data.Random.Distribution.ChiSquare
import Data.Random.Distribution.Normal

import Numeric.SpecFunctions

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
        | otherwise = error "Student's t-distribution: degrees of freedom must be positive"

instance (Real a, Distribution T a) => CDF T a where
    cdf (T n) t = incompleteBeta v2 v2 x
        where
            v = fromIntegral n
            v2 = 0.5 * v
            tD = realToFrac t
            u = sqrt (tD*tD + v)
            x = (tD + u) / (u + u)
