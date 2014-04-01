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
