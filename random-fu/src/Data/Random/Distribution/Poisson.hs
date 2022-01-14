{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, UndecidableInstances
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Poisson where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Binomial

import Control.Monad

import Data.Int
import Data.Word

-- from Knuth, with interpretation help from gsl sources
integralPoisson :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => b -> RVarT m a
integralPoisson = psn 0
    where
        psn :: (Integral a, RealFloat b, Distribution StdUniform b, Distribution (Erlang a) b, Distribution (Binomial b) a) => a -> b -> RVarT m a
        psn j mu
            | mu > 10   = do
                let m = floor (mu * (7/8))

                x <- erlangT m
                if x >= mu
                    then do
                        b <- binomialT (m - 1) (mu / x)
                        return (j + b)
                    else psn (j + m) (mu - x)

            | otherwise = prod 1 j
                where
                    emu = exp (-mu)

                    prod p k = do
                        u <- stdUniformT
                        if p * u > emu
                            then prod (p * u) (k + 1)
                            else return k

integralPoissonCDF :: (Integral a, Real b) => b -> a -> Double
integralPoissonCDF mu k = exp (negate lambda) * sum
    [ exp (fromIntegral i * log lambda - i_fac_ln)
    | (i, i_fac_ln) <- zip [0..k] (scanl (+) 0 (map log [1..]))
    ]

    where lambda = realToFrac mu

-- | The probability of getting exactly k successes is
-- given by the probability mass function:
--
-- \[
-- f(k;\lambda) = \Pr(X = k) = \frac{\lambda^k e^{-\lambda}}{k!}
-- \]
--
-- Note that in `integralPoissonPDF` the parameter of the mass
-- function are given first and the range of the random variable
-- distributed according to the Poisson distribution is given
-- last. That is, \(f(2;0.5)\) is calculated by @integralPoissonPDF 0.5 2@.
integralPoissonPDF :: (Integral a, Real b) => b -> a -> Double
integralPoissonPDF mu k = exp (negate lambda) *
                          exp (fromIntegral k * log lambda - k_fac_ln)
  where
    k_fac_ln = foldl (+) 0 (map (log . fromIntegral) [1..k])
    lambda   = realToFrac mu

fractionalPoisson :: (Num a, Distribution (Poisson b) Integer) => b -> RVarT m a
fractionalPoisson mu = liftM fromInteger (poissonT mu)

fractionalPoissonCDF :: (CDF (Poisson b) Integer, RealFrac a) => b -> a -> Double
fractionalPoissonCDF mu k = cdf (Poisson mu) (floor k :: Integer)

fractionalPoissonPDF :: (PDF (Poisson b) Integer, RealFrac a) => b -> a -> Double
fractionalPoissonPDF mu k = pdf (Poisson mu) (floor k :: Integer)

poisson :: (Distribution (Poisson b) a) => b -> RVar a
poisson mu = rvar (Poisson mu)

poissonT :: (Distribution (Poisson b) a) => b -> RVarT m a
poissonT mu = rvarT (Poisson mu)

newtype Poisson b a = Poisson b

instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Integer) b, Distribution (Binomial b) Integer) => Distribution (Poisson b) Integer where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Integer) => CDF (Poisson b) Integer where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Integer) => PDF (Poisson b) Integer where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int) b, Distribution (Binomial b) Int) => Distribution (Poisson b) Int where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Int) => CDF (Poisson b) Int where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Int) => PDF (Poisson b) Int where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int8) b, Distribution (Binomial b) Int8) => Distribution (Poisson b) Int8 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Int8) => CDF (Poisson b) Int8 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Int8) => PDF (Poisson b) Int8 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int16) b, Distribution (Binomial b) Int16) => Distribution (Poisson b) Int16 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Int16) => CDF (Poisson b) Int16 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Int16) => PDF (Poisson b) Int16 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int32) b, Distribution (Binomial b) Int32) => Distribution (Poisson b) Int32 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Int32) => CDF (Poisson b) Int32 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Int32) => PDF (Poisson b) Int32 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Int64) b, Distribution (Binomial b) Int64) => Distribution (Poisson b) Int64 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Int64) => CDF (Poisson b) Int64 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Int64) => PDF (Poisson b) Int64 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word) b, Distribution (Binomial b) Word) => Distribution (Poisson b) Word where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Word) => CDF (Poisson b) Word where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Word) => PDF (Poisson b) Word where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word8) b, Distribution (Binomial b) Word8) => Distribution (Poisson b) Word8 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Word8) => CDF (Poisson b) Word8 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Word8) => PDF (Poisson b) Word8 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word16) b, Distribution (Binomial b) Word16) => Distribution (Poisson b) Word16 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Word16) => CDF (Poisson b) Word16 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Word16) => PDF (Poisson b) Word16 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word32) b, Distribution (Binomial b) Word32) => Distribution (Poisson b) Word32 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Word32) => CDF (Poisson b) Word32 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Word32) => PDF (Poisson b) Word32 where
    pdf   (Poisson mu) = integralPoissonPDF mu
instance (RealFloat b, Distribution StdUniform b, Distribution (Erlang Word64) b, Distribution (Binomial b) Word64) => Distribution (Poisson b) Word64 where
    rvarT (Poisson mu) = integralPoisson mu
instance (Real b, Distribution (Poisson b) Word64) => CDF (Poisson b) Word64 where
    cdf   (Poisson mu) = integralPoissonCDF mu
instance (Real b, Distribution (Poisson b) Word64) => PDF (Poisson b) Word64 where
    pdf   (Poisson mu) = integralPoissonPDF mu

instance Distribution (Poisson b) Integer => Distribution (Poisson b) Float where
    rvarT (Poisson mu) = fractionalPoisson mu
instance CDF (Poisson b) Integer          => CDF (Poisson b) Float where
    cdf   (Poisson mu) = fractionalPoissonCDF mu
instance PDF (Poisson b) Integer          => PDF (Poisson b) Float where
    pdf   (Poisson mu) = fractionalPoissonPDF mu
instance Distribution (Poisson b) Integer => Distribution (Poisson b) Double where
    rvarT (Poisson mu) = fractionalPoisson mu
instance CDF (Poisson b) Integer          => CDF (Poisson b) Double where
    cdf   (Poisson mu) = fractionalPoissonCDF mu
instance PDF (Poisson b) Integer          => PDF (Poisson b) Double where
    pdf   (Poisson mu) = fractionalPoissonPDF mu
