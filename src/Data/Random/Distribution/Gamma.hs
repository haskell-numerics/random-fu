{-
 -      ``Data/Random/Distribution/Gamma''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -
 -  needs cleanup, verification, and automagic selection of appropriate
 -  algorithms
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

import Control.Monad

data Gamma a
    = Erlang Int a
    | Gamma a a
    | LargeGamma a

instance (Floating a, RealFrac a, Distribution Uniform a) => Distribution Gamma a where
    getDistFrom src (LargeGamma k) = do
        u <- getDistFrom src (Uniform 0 1)
        let y = tan (pi * u)
            x = sqrt (2 * k - 1) * y + k - 1
        
        if x <= 0
            then getDistFrom src (LargeGamma k)
            else do
                v <- getDistFrom src (Uniform 0 1)
                if v > (1 + y*y) * exp ((k-1) * log (x / (k-1)) - sqrt (2 * k - 1) * y)
                    then getDistFrom src (LargeGamma k)
                    else return x
    
    getDistFrom src (Erlang k 1) = do
        -- can this be improved to require fewer than O(k) uniform variables?
        xs <- replicateM k (getDistFrom src (Uniform 0 1))
        return (negate . log . product $ xs)
        
    getDistFrom src (Gamma k 1) = case properFraction k of
        (0,d) -> do
            xi <- getFractionalKDistFrom src d
            return (xi)
            
        (k,0) -> getDistFrom src (Erlang k 1)
            
        (k,d) -> do
            x  <- getDistFrom src (Erlang k 1)
            xi <- getFractionalKDistFrom src d
            return (xi + x)
        
        where getFractionalKDistFrom src d = do
                -- (based on an algorithm in wikipedia... not verified or heavily tested)
                a <- getDistFrom src (Uniform 0 1)
                b <- getDistFrom src (Uniform 0 1)
                c <- getDistFrom src (Uniform 0 1)
                
                let e = exp 1
                    v0 = e / (e + d)
                
                    (xi, eta) = if a <= v0
                        then (b ** (1/d), c * xi ** (d-1))
                        else (1 - log b, c * exp (negate xi))
                
                if eta > xi ** (d-1) * exp (negate xi)
                    then getFractionalKDistFrom src d
                    else return xi
    
    -- scaling
    getDistFrom src (Erlang k theta) = do
        x <- getDistFrom src (Erlang k 1)
        return (x * theta)
        
    getDistFrom src (Gamma k theta) = do
        x <- getDistFrom src (Gamma k 1)
        return (x * theta)
    