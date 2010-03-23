{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Random.Distribution.Multinomial where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Binomial

multinomial :: Distribution (Multinomial p) [a] => [p] -> a -> RVar [a]
multinomial ps n = rvar (Multinomial ps n)

data Multinomial p a where
    Multinomial :: [p] -> a -> Multinomial p [a]

instance (Num a, Fractional p, Distribution (Binomial p) a) => Distribution (Multinomial p) [a] where
    -- TODO: implement faster version based on Categorical for small n, large (length ps)
    rvar (Multinomial ps0 t) = go t ps0 (tailSums ps0) id
        where
            go _ []     _            f = return (f [])
            go n [_]    _            f = return (f [n])
            go 0 (_:ps) (_   :psums) f = go 0 ps psums (f . (0:))
            go n (p:ps) (psum:psums) f = do
                x <- binomial n (p / psum)
                go (n-x) ps psums (f . (x:))
            
            go _ _ _ _ = error "rvar/Multinomial: programming error! this case should be impossible!"
            
            -- less wasteful version of (map sum . tails)
            tailSums [] = [0]
            tailSums (x:xs) = case tailSums xs of
                (s:rest) -> (x+s):s:rest
                _ -> error "rvar/Multinomial/tailSums: programming error! this case should be impossible!"
