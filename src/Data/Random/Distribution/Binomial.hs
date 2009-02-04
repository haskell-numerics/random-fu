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

import Data.Int
import Data.Word
import Control.Monad

    -- algorithm from Knuth's TAOCP, 3rd ed., p 136
    -- specific choice of cutoff size taken from gsl source
integralBinomial :: (Integral a, RealFloat b) => a -> b -> RVar a
integralBinomial t p = bin 0 t p
    where
        bin k t p
            | t > 10    = do
                let a = 1 + t `div` 2
                    b = 1 + t - a
        
                x <- realFloatBetaFromIntegral a b
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

instance (RealFloat b) => Distribution (Binomial b) Int     where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Int8    where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Int16   where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Int32   where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Int64   where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Word8   where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Word16  where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Word32  where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Word64  where rvar (Binomial t p) = integralBinomial t p
instance (RealFloat b) => Distribution (Binomial b) Integer where rvar (Binomial t p) = integralBinomial t p

instance (RealFloat b) => Distribution (Binomial b) Float   where rvar (Binomial t p) = liftM fromInteger (integralBinomial (truncate t) p)
instance (RealFloat b) => Distribution (Binomial b) Double  where rvar (Binomial t p) = liftM fromInteger (integralBinomial (truncate t) p)
