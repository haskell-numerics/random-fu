{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances,
    UndecidableInstances, GADTs
  #-}

module Data.Random.Distribution.Simplex
    ( StdSimplex(..)
    , stdSimplex
    , stdSimplexT
    , fractionalStdSimplex
    ) where

import Control.Monad
import Data.List
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

-- |Uniform distribution over a standard simplex.
newtype StdSimplex as =
    -- | @StdSimplex k@ constructs a standard simplex of dimension @k@
    -- (standard /k/-simplex).
    -- An element of the simplex represents a vector variable @as = (a_0,
    -- a_1, ..., a_k)@. The elements of @as@ are more than or equal to @0@
    -- and @sum as@ is always equal to @1@.
    StdSimplex Int
    deriving (Eq, Show)

instance (Ord a, Fractional a, Distribution StdUniform a) => Distribution StdSimplex [a] where
    rvar (StdSimplex k) = fractionalStdSimplex k

-- |@stdSimplex k@ returns a random variable being uniformly distributed over
-- a standard simplex of dimension @k@.
stdSimplex :: Distribution StdSimplex [a] => Int -> RVar [a]
stdSimplex k = rvar (StdSimplex k)

stdSimplexT :: Distribution StdSimplex [a] => Int -> RVarT m [a]
stdSimplexT k = rvarT (StdSimplex k)

-- |An algorithm proposed by Rubinstein & Melamed (1998).
-- See, /e.g./, S. Onn, I. Weissman.
-- Generating uniform random vectors over a simplex with implications to
-- the volume of a certain polytope and to multivariate extremes.
-- /Ann Oper Res/ (2011) __189__:331-342.
fractionalStdSimplex :: (Ord a, Fractional a, Distribution StdUniform a) => Int -> RVar [a]
fractionalStdSimplex k = do us <- replicateM k stdUniform
                            let us' = sort us ++ [1]
                            return $ zipWith (-) us' (0 : us')
