{-
 -      ``Data/Random/Distribution/Gamma''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -
 -  needs cleanup, verification, and automagic selection of appropriate
 -  algorithms, and proper citations.
 -
 -  should eliminate spurious 'border crossings' betweer RVars and sampleFrom
 -  perhaps Distribution class should have as its basis a function of type
 -  (d t -> RVar t)
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Gamma where

import Data.Random.Source
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal

import Control.Monad

gamma :: (Distribution Gamma a) => a -> a -> RVar a
gamma a b = sample (Gamma a b)

erlang :: (Distribution Gamma a, Integral b, Num a) => b -> a -> RVar a
erlang a b = sample (Gamma (fromIntegral a) b)

data Gamma a
    = Gamma a a

instance (Floating a, RealFrac a, Distribution Uniform a) => Distribution Gamma a where
    -- translated from gsl source - seems to be best I've found by far
    rvar (Gamma a b)
        | a < 1 
        = do
            u <- uniform 0 1
            x <- gamma (1 + a) b
            return (x * u ** recip a)
        | otherwise
        = go
            where
                d = a - (1 / 3)
                c = recip (3 * sqrt d) -- (1 / 3) / sqrt d
                
                go = do
                    (x, _) <- normalPair
                    let cx = c * x
                        v = (1 + cx) ^ 3
                        
                        x_2 = x * x
                        x_4 = x_2 * x_2
                    
                    if cx <= (-1)
                        then go
                        else do
                            u <- uniform 0 1
                            
                            if         u < 1 - 0.0331 * x_4
                                || log u < 0.5 * x_2  + d * (1 - v + log v)
                                then return (b * d * v)
                                else go
