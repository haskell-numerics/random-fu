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

import Control.Monad

data NormalPair a = NormalPair
instance (Floating a, Distribution Uniform a) => Distribution NormalPair (a, a) where
    sampleFrom src NormalPair = do
        u <- sampleFrom src (Uniform 0 1)
        v <- sampleFrom src (Uniform 0 1)
        let r = sqrt (-2 * log u)
            t = 2 * pi * v
            
            x = r * cos t
            y = r * sin t
        return (x,y)

data Normal a
    = StdNormal
    | Normal a a -- mean, sd

instance (Num a, Distribution NormalPair (a,a)) => Distribution Normal a where
    sampleFrom src StdNormal = liftM (fst :: (a,a) -> a) (sampleFrom src NormalPair)
    sampleFrom src (Normal m s) = do
        x <- sampleFrom src StdNormal
        return (x * s + m)
