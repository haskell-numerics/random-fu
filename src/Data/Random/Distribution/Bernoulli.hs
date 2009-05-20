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

bernoulli :: Distribution (Bernoulli b) a => b -> RVar a
bernoulli p = rvar (Bernoulli p)

boolBernoulli :: (Fractional a, Ord a, Distribution Uniform a) => a -> RVar Bool
boolBernoulli p = do
    x <- uniform 0 1
    return (x <= p)

generalBernoulli :: Distribution (Bernoulli b) Bool => a -> a -> b -> RVar a
generalBernoulli t f p = do
    x <- bernoulli p
    return (if x then t else f)

data Bernoulli b a = Bernoulli b

instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Bool        where rvar (Bernoulli p) = boolBernoulli p

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


