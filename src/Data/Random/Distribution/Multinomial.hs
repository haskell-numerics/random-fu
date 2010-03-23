{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Random.Distribution.Multinomial where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Binomial

import Data.List

multinomial :: Distribution (Multinomial p) [a] => [p] -> a -> RVar [a]
multinomial ps n = rvar (Multinomial ps n)

data Multinomial p a where
    Multinomial :: [p] -> a -> Multinomial p [a]

instance (Num a, Fractional p, Distribution (Binomial p) a) => Distribution (Multinomial p) [a] where
    -- TODO: implement faster version based on Categorical for small n, large (length ps)
    rvar (Multinomial ps n) = go n ps (tailSums ps) id
        where
            go n []     _            f = return (f [])
            go n [p]    _            f = return (f [n])
            go 0 (p:ps) (psum:psums) f = go 0 ps psums (f . (0:))
            go n (p:ps) (psum:psums) f = do
                x <- binomial n (p / psum)
                go (n-x) ps psums (f . (x:))
            
            -- less wasteful version of (map sum . tails)
            tailSums [] = [0]
            tailSums (x:xs) = case tailSums xs of
                (s:rest) -> (x+s):s:rest