{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Triangular where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

-- |A description of a triangular distribution - a distribution whose PDF
-- is a triangle ramping up from a lower bound to a specified midpoint
-- and back down to the upper bound.  This is a very simple distribution
-- that does not generally occur naturally but is used sometimes as an
-- estimate of a true distribution when only the range of the values and
-- an approximate mode of the true distribution are known.
data Triangular a = Triangular {
    -- |The lower bound of the triangle in the PDF (the smallest number the distribution can generate)
    triLower  :: a,
    -- |The midpoint of the triangle (also the mode of the distribution)
    triMid    :: a,
    -- |The upper bound of the triangle (and the largest number the distribution can generate)
    triUpper  :: a}
    deriving (Eq, Show)

-- |Compute a triangular distribution for a 'Floating' type.
floatingTriangular :: (Floating a, Ord a, Distribution StdUniform a) => a -> a -> a -> RVarT m a
floatingTriangular a b c
    | a > b     = floatingTriangular b a c
    | b > c     = floatingTriangular a c b
    | otherwise = do
        let p = (c-b)/(c-a)
        u <- stdUniformT
        let d   | u >= p    = a
                | otherwise = c
            x   | u >= p    = (u - p) / (1 - p)
                | otherwise = u / p
-- may prefer this: reusing u costs resolution, especially if p or 1-p is small and c-a is large.
--        x <- stdUniform
        return (b - ((1 - sqrt x) * (b-d)))

-- |@triangularCDF a b c@ is the CDF of @realFloatTriangular a b c@.
triangularCDF :: RealFrac a => a -> a -> a -> a -> Double
triangularCDF a b c x
    | x < a
    = 0
    | x <= b
    = realToFrac ((x - a)^(2 :: Int) / ((c - a) * (b - a)))
    | x <= c
    = realToFrac (1 - (c - x)^(2 :: Int) / ((c - a) * (c - b)))
    | otherwise
    = 1
    
instance (RealFloat a, Ord a, Distribution StdUniform a) => Distribution Triangular a where
    rvarT (Triangular a b c) = floatingTriangular a b c
instance (RealFrac a, Distribution Triangular a) => CDF Triangular a where
    cdf  (Triangular a b c) = triangularCDF a b c