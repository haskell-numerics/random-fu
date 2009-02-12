{-
 -      ``Data/Random/Distribution/Poisson''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, UndecidableInstances
  #-}

module Data.Random.Distribution.Poisson where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Binomial

import Data.Int
import Data.Word

import Control.Monad

-- from Knuth, with interpretation help from gsl sources
integralPoisson :: (Integral a, RealFloat b) => b -> RVar a
integralPoisson mu = psn 0 mu
    where
        psn k mu
            | mu > 10   = do
                let m = floor (mu * (7/8))
            
                x <- realFloatErlang m
                if x >= mu
                    then do
                        b <- integralBinomial (m - 1) (mu / x)
                        return (k + b)
                    else psn (k + m) (mu - x)
            
            | otherwise = prod 1 k
                where
                    emu = exp (-mu)
                
                    prod p k = do
                        u <- realFloatStdUniform
                        if p * u > emu
                            then prod (p * u) (k + 1)
                            else return k


poisson :: (Distribution (Poisson b) a) => b -> RVar a
poisson mu = sample (Poisson mu)

data Poisson b a = Poisson b

instance RealFloat b => Distribution (Poisson b) Int        where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Int8       where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Int16      where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Int32      where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Int64      where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Word8      where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Word16     where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Word32     where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Word64     where rvar (Poisson mu) = integralPoisson mu
instance RealFloat b => Distribution (Poisson b) Integer    where rvar (Poisson mu) = integralPoisson mu

instance RealFloat b => Distribution (Poisson b) Float      where rvar (Poisson mu) = liftM fromIntegral (integralPoisson mu)
instance RealFloat b => Distribution (Poisson b) Double     where rvar (Poisson mu) = liftM fromIntegral (integralPoisson mu)
