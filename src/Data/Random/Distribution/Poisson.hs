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
integralPoisson :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => b -> RVar a
integralPoisson mu = psn 0 mu
    where
        psn :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => a -> b -> RVar a
        psn k mu
            | mu > 10   = do
                let m = floor (mu * (7/8))
            
                x <- erlang m
                if x >= mu
                    then do
                        b <- binomial (m - 1) (mu / x)
                        return (k + b)
                    else psn (k + m) (mu - x)
            
            | otherwise = prod 1 k
                where
                    emu = exp (-mu)
                
                    prod p k = do
                        u <- stdUniform
                        if p * u > emu
                            then prod (p * u) (k + 1)
                            else return k

fractionalPoisson :: (Num a, Distribution (Poisson b) Integer) => b -> RVar a
fractionalPoisson mu = liftM fromInteger (poisson mu)

poisson :: (Distribution (Poisson b) a) => b -> RVar a
poisson mu = rvar (Poisson mu)

data Poisson b a = Poisson b

-- so ugly...
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int     ) b, Distribution (Binomial b) Int     ) => Distribution (Poisson b) Int        where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int8    ) b, Distribution (Binomial b) Int8    ) => Distribution (Poisson b) Int8       where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int16   ) b, Distribution (Binomial b) Int16   ) => Distribution (Poisson b) Int16      where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int32   ) b, Distribution (Binomial b) Int32   ) => Distribution (Poisson b) Int32      where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int64   ) b, Distribution (Binomial b) Int64   ) => Distribution (Poisson b) Int64      where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word8   ) b, Distribution (Binomial b) Word8   ) => Distribution (Poisson b) Word8      where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word16  ) b, Distribution (Binomial b) Word16  ) => Distribution (Poisson b) Word16     where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word32  ) b, Distribution (Binomial b) Word32  ) => Distribution (Poisson b) Word32     where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word64  ) b, Distribution (Binomial b) Word64  ) => Distribution (Poisson b) Word64     where rvar (Poisson mu) = integralPoisson mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Integer ) b, Distribution (Binomial b) Integer ) => Distribution (Poisson b) Integer    where rvar (Poisson mu) = integralPoisson mu

instance (Distribution (Poisson b) Integer) => Distribution (Poisson b) Float            where rvar (Poisson mu) = fractionalPoisson mu
instance (Distribution (Poisson b) Integer) => Distribution (Poisson b) Double           where rvar (Poisson mu) = fractionalPoisson mu
