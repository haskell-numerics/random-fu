{-
 -      ``Data/Random/Distribution/Gamma''
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
    UndecidableInstances, BangPatterns
  #-}

module Data.Random.Distribution.Gamma where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal

import Control.Monad
import Data.Ratio

-- derived from  Marsaglia & Tang, "A Simple Method for generating gamma
-- variables", ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372.
-- 'd' and 'c' are promoted from locally-computed constants to parameters of the
-- distribution.
{-# SPECIALIZE mtGamma :: Double -> Double -> RVar Double #-}
{-# SPECIALIZE mtGamma :: Float -> Float -> RVar Float #-}
mtGamma
    :: (Floating a, Ord a,
        Distribution StdUniform a, 
        Distribution Normal a)
    => a -> a -> RVar a
mtGamma a b = go
    where
        !d = a - fromRational (1%3)
        !c = recip (sqrt (9*d))
        
        go = do
            x <- stdNormal
            let !v  = (1 + c*x)^3
            
            if v <= 0
                then go
                else do
                    u  <- stdUniform
                    let !x_2 = x*x; !x_4 = x_2*x_2
                        dv = d * v
                    if      u < 1 - 0.0331*x_4
                     || log u < 0.5 * x_2 + d - dv + d*log v
                        then return (b*dv)
                        else go


gamma :: (Distribution Gamma a) => a -> a -> RVar a
gamma a b = rvar (Gamma a b)

erlang :: (Distribution (Erlang a) b) => a -> RVar b
erlang a = rvar (Erlang a)

data Gamma a    = Gamma a a
data Erlang a b = Erlang a

instance (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a) => Distribution Gamma a where
    {-# SPECIALIZE instance Distribution Gamma Double #-}
    {-# SPECIALIZE instance Distribution Gamma Float #-}
    rvar (Gamma a b) = mtGamma a b

instance (Integral a, Floating b, Ord b, Distribution Normal b, Distribution StdUniform b) => Distribution (Erlang a) b where
    rvar (Erlang a) = mtGamma (fromIntegral a) 1
