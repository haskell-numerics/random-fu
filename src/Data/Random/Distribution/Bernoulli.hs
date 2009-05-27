{-
 -      ``Data/Random/Distribution/Bernoulli''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Bernoulli where

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

instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Int         where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Int8        where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Int16       where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Int32       where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Int64       where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Word8       where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Word16      where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Word32      where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Word64      where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Integer     where rvar (Bernoulli p) = generalBernoulli 0 1 p

instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Float       where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance Distribution (Bernoulli b) Bool => Distribution (Bernoulli b) Double      where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance (Distribution (Bernoulli b) Bool, Integral a)
    => Distribution (Bernoulli b) (Ratio a)   
    where rvar (Bernoulli p) = generalBernoulli 0 1 p
instance (Distribution (Bernoulli b) Bool, RealFloat a)
    => Distribution (Bernoulli b) (Complex a)
    where rvar (Bernoulli p) = generalBernoulli 0 1 p


