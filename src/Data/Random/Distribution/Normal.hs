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

-- Box-Muller method
normalPair :: (Floating a, Distribution Uniform a) => RVarT m (a,a)
normalPair = do
    u <- uniform 0 1
    t <- uniform 0 (2 * pi)
    let r = sqrt (-2 * log u)
        
        x = r * cos t
        y = r * sin t
    return (x,y)

-- slightly slower
knuthPolarNormalPair :: (Floating a, Ord a, Distribution Uniform a) => RVarT m (a,a)
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

realFloatStdNormal :: RealFloat a => RVarT m a
realFloatStdNormal = do
    u <- realFloatStdUniform
    t <- realFloatStdUniform
    let r = sqrt (-2 * log u)
        
        x = r * cos (t * 2 * pi)
    return x
    

data Normal a
    = StdNormal
    | Normal a a -- mean, sd

instance (Floating a, Distribution Uniform a) => Distribution Normal a where
    rvarT StdNormal = liftM fst normalPair
    rvarT (Normal m s) = do
        x <- liftM fst normalPair
        return (x * s + m)

stdNormal :: Distribution Normal a => RVarT m a
stdNormal = rvarT StdNormal

normal :: Distribution Normal a => a -> a -> RVarT m a
normal m s = rvarT (Normal m s)