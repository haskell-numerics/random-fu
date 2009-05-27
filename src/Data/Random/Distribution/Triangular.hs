{-
 -      ``Data/Random/Distribution/Triangular''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Triangular where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

data Triangular a = Triangular
    { triLower  :: a
    , triMid    :: a
    , triUpper  :: a
    } deriving (Eq, Show)

realFloatTriangular :: (Floating a, Ord a, Distribution StdUniform a) => a -> a -> a -> RVar a
realFloatTriangular a b c
    | a <= b && b <= c
    = do
        let p = (c-b)/(c-a)
        u <- stdUniform
        let d   | u >= p    = a
                | otherwise = c
            x   | u >= p    = (u - p) / (1 - p)
                | otherwise = u / p
-- may prefer this: reusing u costs resolution, especially if p or 1-p is small and c-a is large.
--        x <- stdUniform
        return (b - ((1 - sqrt x) * (b-d)))

realFloatTriangularCDF :: RealFrac a => a -> a -> a -> a -> Double
realFloatTriangularCDF a b c x
    | x < a
    = 0
    | x <= b
    = realToFrac ((x - a) ^ 2 / ((c - a) * (b - a)))
    | x <= c
    = realToFrac (1 - (c - x) ^ 2 / ((c - a) * (c - b)))
    | otherwise
    = 1
    
instance (RealFloat a, Ord a, Distribution StdUniform a) => Distribution Triangular a where
    rvar (Triangular a b c) = realFloatTriangular a b c
instance (RealFrac a, Distribution Triangular a) => CDF Triangular a where
    cdf  (Triangular a b c) = realFloatTriangularCDF a b c