{-
 -      ``Data/Random/Distribution/Bernoulli''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances,
    TemplateHaskell
  #-}

module Data.Random.Distribution.Bernoulli where

import Data.Random.Internal.TH

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Uniform

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex

-- |Generate a Bernoulli variate with the given probability.  For @Bool@ results,
-- @bernoulli p@ will return True (p*100)% of the time and False otherwise.
-- For numerical types, True is replaced by 1 and False by 0.
bernoulli :: Distribution (Bernoulli b) a => b -> RVar a
bernoulli p = rvar (Bernoulli p)

-- |A random variable whose value is 'True' the given fraction of the time
-- and 'False' the rest.
boolBernoulli :: (Fractional a, Ord a, Distribution StdUniform a) => a -> RVar Bool
boolBernoulli p = do
    x <- stdUniform
    return (x <= p)

boolBernoulliCDF :: (Real a) => a -> Bool -> Double
boolBernoulliCDF p True  = 1
boolBernoulliCDF p False = (1 - realToFrac p)

-- | @generalBernoulli t f p@ generates a random variable whose value is @t@
-- with probability @p@ and @f@ with probability @1-p@.
generalBernoulli :: Distribution (Bernoulli b) Bool => a -> a -> b -> RVar a
generalBernoulli f t p = do
    x <- bernoulli p
    return (if x then t else f)

generalBernoulliCDF :: CDF (Bernoulli b) Bool => (a -> a -> Bool) -> a -> a -> b -> a -> Double
generalBernoulliCDF (>=) f t p x
    | f >= t    = error "generalBernoulliCDF: f >= t"
    | x >= t    = cdf (Bernoulli p) True
    | x >= f    = cdf (Bernoulli p) False
    | otherwise = 0

data Bernoulli b a = Bernoulli b

instance (Fractional b, Ord b, Distribution StdUniform b) 
       => Distribution (Bernoulli b) Bool
    where
        rvar (Bernoulli p) = boolBernoulli p
instance (Distribution (Bernoulli b) Bool, Real b)
       => CDF (Bernoulli b) Bool
    where
        cdf  (Bernoulli p) = boolBernoulliCDF p

$( replicateInstances ''Int integralTypes [d|
        instance Distribution (Bernoulli b) Bool 
              => Distribution (Bernoulli b) Int
              where
                  rvar (Bernoulli p) = generalBernoulli 0 1 p
        instance CDF (Bernoulli b) Bool
              => CDF (Bernoulli b) Int
              where
                  cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
    |] )

$( replicateInstances ''Float realFloatTypes [d|
        instance Distribution (Bernoulli b) Bool 
              => Distribution (Bernoulli b) Float
              where
                  rvar (Bernoulli p) = generalBernoulli 0 1 p
        instance CDF (Bernoulli b) Bool
              => CDF (Bernoulli b) Float
              where
                  cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
    |] )

instance (Distribution (Bernoulli b) Bool, Integral a)
       => Distribution (Bernoulli b) (Ratio a)   
       where
           rvar (Bernoulli p) = generalBernoulli 0 1 p
instance (CDF (Bernoulli b) Bool, Integral a)
       => CDF (Bernoulli b) (Ratio a)   
       where
           cdf  (Bernoulli p) = generalBernoulliCDF (>=) 0 1 p
instance (Distribution (Bernoulli b) Bool, RealFloat a)
       => Distribution (Bernoulli b) (Complex a)
       where
           rvar (Bernoulli p) = generalBernoulli 0 1 p
instance (CDF (Bernoulli b) Bool, RealFloat a)
       => CDF (Bernoulli b) (Complex a)
       where
           cdf  (Bernoulli p) = generalBernoulliCDF (\x y -> realPart x >= realPart y) 0 1 p
