{-
 -      ``Data/Random/Distribution/Gamma''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -
 -  needs cleanup, verification, and automagic selection of appropriate
 -  algorithms, and proper citations
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Gamma where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal

import Control.Monad

data Gamma a
    = Erlang Int a
    | Gamma a a
    | LargeGamma a

data MTGamma a
    = MTGamma a a

instance (Floating a, RealFrac a, Distribution Uniform a) => Distribution Gamma a where
    sampleFrom src (LargeGamma k) = do
        u <- sampleFrom src (Uniform 0 1)
        let y = tan (pi * u)
            x = sqrt (2 * k - 1) * y + k - 1
        
        if x <= 0
            then sampleFrom src (LargeGamma k)
            else do
                v <- sampleFrom src (Uniform 0 1)
                if v > (1 + y*y) * exp ((k-1) * log (x / (k-1)) - sqrt (2 * k - 1) * y)
                    then sampleFrom src (LargeGamma k)
                    else return x
    
    sampleFrom src (Erlang k 1) = do
        -- can this be improved to require fewer than O(k) uniform variables?
        xs <- replicateM k (sampleFrom src (Uniform 0 1))
        return (negate . log . product $ xs)
        
    sampleFrom src (Gamma k 1) = case properFraction k of
        (0,d) -> do
            xi <- getFractionalKDistFrom src d
            return (xi)
            
        (k,0) -> sampleFrom src (Erlang k 1)
            
        (k,d) -> do
            x  <- sampleFrom src (Erlang k 1)
            xi <- getFractionalKDistFrom src d
            return (xi + x)
        
        where getFractionalKDistFrom src d = do
                -- (based on an algorithm in wikipedia... not verified or heavily tested)
                a <- sampleFrom src (Uniform 0 1)
                b <- sampleFrom src (Uniform 0 1)
                c <- sampleFrom src (Uniform 0 1)
                
                let e = exp 1
                    v0 = e / (e + d)
                
                    (xi, eta) = if a <= v0
                        then (b ** (1/d), c * xi ** (d-1))
                        else (1 - log b, c * exp (negate xi))
                
                if eta > xi ** (d-1) * exp (negate xi)
                    then getFractionalKDistFrom src d
                    else return xi
    
    -- scaling
    sampleFrom src (Erlang k theta) = do
        x <- sampleFrom src (Erlang k 1)
        return (x * theta)
        
    sampleFrom src (Gamma k theta) = do
        x <- sampleFrom src (Gamma k 1)
        return (x * theta)
    
instance (Floating a, RealFrac a, Distribution Uniform a, Distribution Normal a) => Distribution MTGamma a where
    -- translated from gsl source - seems to be best I've found by far
    sampleFrom src (MTGamma a b)
        | a < 1 
        = do
            u <- sampleFrom src (Uniform 0 1)
            x <- sampleFrom src (MTGamma (1 + a) b)
            return (x * u ** recip a)
        | otherwise
        = go
            where
                d = a - (1 / 3)
                c = recip (3 * sqrt d) -- (1 / 3) / sqrt d
                
                go = do
                    x <- sampleFrom src StdNormal
                    let cx = c * x
                        v = (1 + cx) ^ 3
                        
                        x_2 = x * x
                        x_4 = x_2 * x_2
                    
                    if cx <= (-1)
                        then go
                        else do
                            u <- sampleFrom src (Uniform 0 1)
                            
                            if         u < 1 - 0.0331 * x_4
                                || log u < 0.5 * x_2  + d * (1 - v + log v)
                                then return (b * d * v)
                                else go
