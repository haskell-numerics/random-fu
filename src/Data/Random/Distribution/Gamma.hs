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
    UndecidableInstances
  #-}

module Data.Random.Distribution.Gamma where

import Data.Random.Source
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal

import Control.Monad

    -- translated from gsl source - seems to be best I've found by far.
    -- originally comes from Marsaglia & Tang, "A Simple Method for
    -- generating gamma variables", ACM Transactions on Mathematical
    -- Software, Vol 26, No 3 (2000), p363-372.
realFloatGamma :: (Floating a, Ord a, Distribution NormalPair (a,a), Distribution StdUniform a) => a -> a -> RVar a
realFloatGamma a b
    | a < 1 
    = do
        u <- stdUniform
        x <- realFloatGamma (1 + a) b
        return (x * u ** recip a)
    | otherwise
    = goNothing
        where
            d = a - (1 / 3)
            c = recip (3 * sqrt d) -- (1 / 3) / sqrt d
            
            -- manually unrolled StateT (Maybe Double)
            -- since the current stdNormal uses a normal pair and
            -- discards the 2nd, this implementation uses
            -- the normal pair and stashes one so that every other
            -- time through the loop it can recycle what
            -- would be discarded anyway.
            goNothing = do
                (x, stashed) <- normalPair
                step x (goJust stashed)
                
            goJust x = step x goNothing
            
            step x next = do
                let cx = c * x
                    v = (1 + cx) ^ 3
                    
                    x_2 = x * x
                    x_4 = x_2 * x_2
                
                if cx <= (-1)
                    then next
                    else do
                        u <- stdUniform
                        
                        if         u < 1 - 0.0331 * x_4
                            || log u < 0.5 * x_2  + d * (1 - v + log v)
                            then return (b * d * v)
                            else next


realFloatErlang :: (Integral a, Floating b, Ord b, Distribution NormalPair (b,b), Distribution StdUniform b) => a -> RVar b
realFloatErlang a
    | a < 1 
    = fail "realFloatErlang: a < 1"
    | otherwise
    = goNothing
        where
            d = fromIntegral a - (1 / 3)
            c = recip (3 * sqrt d) -- (1 / 3) / sqrt d
            
            goNothing = do
                (x, stashed) <- normalPair
                step x (goJust stashed)
                
            goJust x = step x goNothing
            
            step x next = do
                let cx = c * x
                    v = (1 + cx) ^ 3
                    
                    x_2 = x * x
                    x_4 = x_2 * x_2
                
                if cx <= (-1)
                    then next
                    else do
                        u <- stdUniform
                        
                        if         u < 1 - 0.0331 * x_4
                            || log u < 0.5 * x_2  + d * (1 - v + log v)
                            then return (d * v)
                            else next

gamma :: (Distribution Gamma a) => a -> a -> RVar a
gamma a b = rvar (Gamma a b)

erlang :: (Distribution (Erlang a) b) => a -> RVar b
erlang a = rvar (Erlang a)

data Gamma a    = Gamma a a
data Erlang a b = Erlang a

instance (Floating a, Ord a, Distribution NormalPair (a,a), Distribution StdUniform a) => Distribution Gamma a where
    rvar (Gamma a b) = realFloatGamma a b

instance (Integral a, Floating b, Ord b, Distribution NormalPair (b,b), Distribution StdUniform b) => Distribution (Erlang a) b where
    rvar (Erlang a) = realFloatErlang a