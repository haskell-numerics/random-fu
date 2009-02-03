{-
 -      ``Data/Random/Distribution/Binomial''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Binomial where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform

integralBinomial :: (Integral a, RealFloat b) => a -> b -> RVar a
integralBinomial t p = bin 0 t p
    where
        bin k t p
            | t > 10    = do
                let a = 1 + t `div` 2
                    b = 1 + t - a
        
                x <- realFloatBeta (fromIntegral a) (fromIntegral b)
                if x >= p
                    then bin  k      (a - 1) (p / x)
                    else bin (k + a) (b - 1) ((p - x) / (1 - x))
        
            | otherwise = count k t
                where
                    count k  0    = return k
                    count k (n+1) = do
                        x <- realFloatStdUniform
                        (count $! (if x < p then k + 1 else k)) n



binomial :: Distribution (Binomial b) a => a -> b -> RVar a
binomial t p = sample (Binomial t p)

data Binomial b a = Binomial a b

instance (Integral a, RealFloat b) => Distribution (Binomial b) a where
    -- algorithm from Knuth's TAOCP, 3rd ed., p 136
    -- specific choice of cutoff size taken from gsl source
    rvar (Binomial t p) = integralBinomial t p