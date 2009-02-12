{-
 -      ``Data/Random/Distribution/Triangular''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances
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

realFloatTriangular :: (RealFloat a) => a -> a -> a -> RVar a
realFloatTriangular a b c
    | a <= b && b <= c
    = do
        let p = (c-b)/(c-a)
        u <- realFloatStdUniform
        let d   | u >= p    = a
                | otherwise = c
            x   | u >= p    = (u - p) / (1 - p)
                | otherwise = u / p
-- may prefer this: reusing u costs resolution, especially if p or 1-p is small and c-a is large.
--        x <- realFloatStdUniform
        return (b - ((1 - sqrt x) * (b-d)))

instance RealFloat a => Distribution Triangular a where
    rvar (Triangular a b c) = realFloatTriangular a b c