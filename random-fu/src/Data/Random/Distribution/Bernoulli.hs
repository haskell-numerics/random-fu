{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances,
    TemplateHaskell
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Bernoulli where

import Data.Random.Internal.TH

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

import Data.Ratio
import Data.Complex

-- |Generate a Bernoulli variate with the given probability.  For @Bool@ results,
-- @bernoulli p@ will return True (p*100)% of the time and False otherwise.
-- For numerical types, True is replaced by 1 and False by 0.
bernoulli :: Distribution (Bernoulli b) a => b -> RVar a
bernoulli p = rvar (Bernoulli p)

-- |Generate a Bernoulli process with the given probability.  For @Bool@ results,
-- @bernoulli p@ will return True (p*100)% of the time and False otherwise.
-- For numerical types, True is replaced by 1 and False by 0.
bernoulliT :: (Distribution (Bernoulli b) a, Functor m) => b -> RVarT m a
bernoulliT p = rvarT (Bernoulli p)

-- |A random variable whose value is 'True' the given fraction of the time
-- and 'False' the rest.
boolBernoulli :: (Fractional a, Ord a, Distribution StdUniform a, Functor m) => a -> RVarT m Bool
boolBernoulli p = do
    x <- stdUniformT
    return (x <= p)

boolBernoulliCDF :: (Real a) => a -> Bool -> Double
boolBernoulliCDF _ True  = 1
boolBernoulliCDF p False = (1 - realToFrac p)

-- | @generalBernoulli t f p@ generates a random variable whose value is @t@
-- with probability @p@ and @f@ with probability @1-p@.
generalBernoulli :: (Distribution (Bernoulli b) Bool, Functor m) => a -> a -> b -> RVarT m a
generalBernoulli f t p = do
    x <- bernoulliT p
    return (if x then t else f)

generalBernoulliCDF :: CDF (Bernoulli b) Bool => (a -> a -> Bool) -> a -> a -> b -> a -> Double
generalBernoulliCDF gte f t p x
    | f `gte` t = error "generalBernoulliCDF: f >= t"
    | x `gte` t = cdf (Bernoulli p) True
    | x `gte` f = cdf (Bernoulli p) False
    | otherwise = 0

newtype Bernoulli b a = Bernoulli b

instance (Fractional b, Ord b, Distribution StdUniform b)
       => Distribution (Bernoulli b) Bool
    where
        rvarT (Bernoulli p) = boolBernoulli p
instance (Distribution (Bernoulli b) Bool, Real b)
       => CDF (Bernoulli b) Bool
    where
        cdf  (Bernoulli p) = boolBernoulliCDF p

$( replicateInstances ''Int integralTypes [d|
        instance Distribution (Bernoulli b) Bool
              => Distribution (Bernoulli b) Int
              where
                  rvarT (Bernoulli p) = generalBernoulli 0 1 p
        instance CDF (Bernoulli b) Bool
              => CDF (Bernoulli b) Int
              where
                  cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
    |] )

$( replicateInstances ''Float realFloatTypes [d|
        instance Distribution (Bernoulli b) Bool
              => Distribution (Bernoulli b) Float
              where
                  rvarT (Bernoulli p) = generalBernoulli 0 1 p
        instance CDF (Bernoulli b) Bool
              => CDF (Bernoulli b) Float
              where
                  cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
    |] )

instance (Distribution (Bernoulli b) Bool, Integral a)
       => Distribution (Bernoulli b) (Ratio a)
       where
           rvarT (Bernoulli p) = generalBernoulli 0 1 p
instance (CDF (Bernoulli b) Bool, Integral a)
       => CDF (Bernoulli b) (Ratio a)
       where
           cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
instance (Distribution (Bernoulli b) Bool, RealFloat a)
       => Distribution (Bernoulli b) (Complex a)
       where
           rvarT (Bernoulli p) = generalBernoulli 0 1 p
instance (CDF (Bernoulli b) Bool, RealFloat a)
       => CDF (Bernoulli b) (Complex a)
       where
           cdf  (Bernoulli p) = generalBernoulliCDF (\x y -> realPart x >= realPart y) 0 1 p
