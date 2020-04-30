{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Pareto where

import Data.Random

pareto :: Distribution Pareto a => a -> a -> RVar a
pareto xM a = rvar (Pareto xM a)

paretoT :: Distribution Pareto a => a -> a -> RVarT m a
paretoT xM a = rvarT (Pareto xM a)

data Pareto a = Pareto !a !a

instance (Floating a, Distribution StdUniform a) => Distribution Pareto a where
    rvarT (Pareto xM a) = do
        u <- stdUniformT
        return (xM / (1 - u) ** recip a)

instance (Real a, Distribution Pareto a) => CDF Pareto a where
    cdf (Pareto xM a) x
         | x >= xM      = 1 - (realToFrac xM / realToFrac x) ** realToFrac a
         | otherwise    = 0
