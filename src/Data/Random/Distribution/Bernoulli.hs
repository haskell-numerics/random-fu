{-
 -      ``Data/Random/Distribution/Bernoulli''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Bernoulli where

import Data.Random.Internal.Classification

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Random.Distribution.Uniform

import Data.Int
import Data.Word

bernoulli :: Distribution (Bernoulli b) a => b -> RVarT m a
bernoulli p = rvarT (Bernoulli p)

boolBernoulli p = do
    x <- realFloatUniform 0 1
    return (x <= p)

generalBernoulli t f p = do
    x <- boolBernoulli p
    return (if x then t else f)

class (Classification NumericType t c) => BernoulliByClassification c t where
    bernoulliByClassification :: RealFloat a => a -> RVarT m t

instance (Classification NumericType t IntegralType, Num t) => BernoulliByClassification IntegralType t
    where bernoulliByClassification = generalBernoulli 0 1
instance (Classification NumericType t FractionalType, Num t) => BernoulliByClassification FractionalType t
    where bernoulliByClassification = generalBernoulli 0 1
instance (Classification NumericType t EnumType, Enum t) => BernoulliByClassification EnumType t
    where bernoulliByClassification = generalBernoulli (toEnum 0) (toEnum 1)

data Bernoulli b a = Bernoulli b

instance (BernoulliByClassification c t, RealFloat b) => Distribution (Bernoulli b) t where
    rvarT (Bernoulli p) = bernoulliByClassification p
