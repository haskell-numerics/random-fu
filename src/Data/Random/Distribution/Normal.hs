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

normalPair :: Distribution NormalPair (a, a) => RVar (a,a)
normalPair = rvar NormalPair

-- Box-Muller method
{-# INLINE boxMullerNormalPair #-}
boxMullerNormalPair :: (Floating a, Distribution StdUniform a) => RVar (a,a)
boxMullerNormalPair = do
    u <- stdUniform
    t <- stdUniform
    let r = sqrt (-2 * log u)
        theta = (2 * pi) * t
        
        x = r * cos theta
        y = r * sin theta
    return (x,y)

-- slightly slower
knuthPolarNormalPair :: (Floating a, Ord a, Distribution Uniform a) => RVar (a,a)
knuthPolarNormalPair = do
    v1 <- uniform (-1) 1
    v2 <- uniform (-1) 1
    
    let s = v1*v1 + v2*v2
    if s >= 1
        then knuthPolarNormalPair
        else return $ if s == 0
            then (0,0)
            else let scale = sqrt (-2 * log s / s) 
                  in (v1 * scale, v2 * scale)

realFloatStdNormal :: RealFloat a => RVar a
realFloatStdNormal = do
    u <- realFloatStdUniform
    t <- realFloatStdUniform
    let r = sqrt (-2 * log u)
        
        x = r * cos (t * 2 * pi)
    return x
    

doubleStdNormal :: RVar Double
doubleStdNormal = do
    u <- doubleStdUniform
    t <- doubleStdUniform
    let r = sqrt (-2 * log u)
        
        x = r * cos (t * 2 * pi)
    return x
    

data Normal a
    = StdNormal
    | Normal a a -- mean, sd

data NormalPair a = NormalPair

instance (Floating a, Distribution NormalPair (a,a)) => Distribution Normal a where
    rvar StdNormal = liftM fst normalPair
    rvar (Normal m s) = do
        x <- liftM fst normalPair
        return (x * s + m)

instance (Floating a, Distribution StdUniform a) => Distribution NormalPair (a, a) where
    rvar _ = boxMullerNormalPair

stdNormal :: Distribution Normal a => RVar a
stdNormal = rvar StdNormal

normal :: Distribution Normal a => a -> a -> RVar a
normal m s = rvar (Normal m s)