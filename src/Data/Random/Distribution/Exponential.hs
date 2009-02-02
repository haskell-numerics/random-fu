{-
 -      ``Data/Random/Distribution/Exponential''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Exponential where

import Data.Random.Source
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

data Exponential a = Exp a

exponential :: Distribution Exponential a => a -> RVar a
exponential = sample . Exp

instance (Floating a, Distribution Uniform a) => 
    Distribution Exponential a where
        rvar (Exp lambdaRecip) = do
            x <- uniform 0 1
            return (negate (log x) * lambdaRecip)
