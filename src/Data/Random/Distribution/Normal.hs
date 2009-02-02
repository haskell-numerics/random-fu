{-
 -      ``Data/Random/Distribution/Normal''
 -  
 -  Quick and dirty implementation - eventually something faster probably 
 -  ought to be implemented instead.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Normal where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.RVar

import Control.Monad

normalPair :: (Floating a, Distribution Uniform a) => RVar (a,a)
normalPair = do
        u <- uniform 0 1
        t <- uniform 0 (2 * pi)
        let r = sqrt (-2 * log u)
            
            x = r * cos t
            y = r * sin t
        return (x,y)
    

data Normal a
    = StdNormal
    | Normal a a -- mean, sd

instance (Floating a, Distribution Uniform a) => Distribution Normal a where
    rvar StdNormal = liftM fst normalPair
    rvar (Normal m s) = do
        x <- liftM fst normalPair
        return (x * s + m)

stdNormal :: Distribution Normal a => RVar a
stdNormal = sample StdNormal

normal :: Distribution Normal a => a -> a -> RVar a
normal m s = sample (Normal m s)