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

fractionalDirichlet :: (Fractional a, Distribution Gamma a) => [a] -> RVar [a]
fractionalDirichlet []  = return []
fractionalDirichlet [_] = return [1]
fractionalDirichlet as = do
    xs <- sequence [gamma a 1 | a <- as]
    let total = foldl1' (+) xs
    
    return (map (* recip total) xs)

dirichlet :: Distribution Dirichlet [a] => [a] -> RVar [a]
dirichlet as = rvar (Dirichlet as)

newtype Dirichlet a = Dirichlet a deriving (Eq, Show)

instance (Fractional a, Distribution Gamma a) => Distribution Dirichlet [a] where
    rvar (Dirichlet as) = fractionalDirichlet as
