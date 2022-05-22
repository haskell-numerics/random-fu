{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances, GADTs
  #-}

module Data.Random.Distribution.Dirichlet where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Gamma

import Data.List

fractionalDirichlet :: (Fractional a, Distribution Gamma a, Functor m) => [a] -> RVarT m [a]
fractionalDirichlet []  = return []
fractionalDirichlet [_] = return [1]
fractionalDirichlet as = do
    xs <- sequence [gammaT a 1 | a <- as]
    let total = foldl1' (+) xs

    return (map (* recip total) xs)

dirichlet :: Distribution Dirichlet [a] => [a] -> RVar [a]
dirichlet as = rvar (Dirichlet as)

dirichletT :: (Distribution Dirichlet [a], Functor m) => [a] -> RVarT m [a]
dirichletT as = rvarT (Dirichlet as)

newtype Dirichlet a = Dirichlet a deriving (Eq, Show)

instance (Fractional a, Distribution Gamma a) => Distribution Dirichlet [a] where
    rvarT (Dirichlet as) = fractionalDirichlet as
