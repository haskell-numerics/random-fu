{-
 -      ``Data/Random/Distribution/Discrete''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Discrete where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

import Control.Monad

discrete :: Distribution (Discrete p) a => [(p,a)] -> RVar a
discrete ps = rvar (Discrete ps)

data Discrete p a = Discrete [(p, a)]

instance (Num p, Ord p, Distribution Uniform p) => Distribution (Discrete p) a where
    rvar (Discrete []) = fail "discrete distribution over empty set cannot be sampled"
    rvar (Discrete ds) = do
        let (ps, xs) = unzip ds
            cs = scanl1 (+) ps
        
        when (any (<0) ps) $ fail "negative probability in discrete distribution"
        
        u <- uniform 0 (last cs)
        return $ head
            [ x
            | (c,x) <- zip cs xs
            , c >= u
            ]