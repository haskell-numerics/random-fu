{-
 -      ``Data/Random/Distribution/Bernoulli''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Bernoulli where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Uniform

import Data.Int
import Data.Word

bernoulli :: (Distribution (Bernoulli b) a) => b -> RVar a
bernoulli p = sample (Bernoulli p)

boolBernoulli p = do
    x <- uniform 0 1
    return (x <= p)

generalBernoulli p t f = do
    x <- boolBernoulli p
    return (if x then t else f)

data Bernoulli b a = Bernoulli b

instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Bool where
    rvar (Bernoulli p) = boolBernoulli p

instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Int        where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Int8       where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Int16      where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Int32      where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Int64      where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Word8      where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Word16     where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Word32     where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Word64     where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Integer    where rvar (Bernoulli p) = generalBernoulli p 0 1

instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Float      where rvar (Bernoulli p) = generalBernoulli p 0 1
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) Double     where rvar (Bernoulli p) = generalBernoulli p 0 1

instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) (a -> Either a a) where
    rvar (Bernoulli p) = generalBernoulli p Left Right
instance (Fractional b, Ord b, Distribution Uniform b) => Distribution (Bernoulli b) ((a,a) -> a) where
    rvar (Bernoulli p) = generalBernoulli p fst snd

