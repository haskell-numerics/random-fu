{-
 -      ``Data/Random''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.Random
    ( module Data.Random.Source
    , module Data.Random.Source.DevRandom
    , module Data.Random.Source.Old
    , module Data.Random.Source.PreFetch
    , module Data.Random.Source.Std
    , module Data.Random.Distribution
    , module Data.Random.Distribution.Beta
    , module Data.Random.Distribution.Gamma
    , module Data.Random.Distribution.Exponential
    , module Data.Random.Distribution.Normal
    , module Data.Random.Distribution.Uniform
    , module Data.Random.RVar
    ) where

import Data.Random.Source
import Data.Random.Source.DevRandom
import Data.Random.Source.Old
import Data.Random.Source.PreFetch
import Data.Random.Source.Std
import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Data.Random.RVar

-- some convenient testing stuff
import Data.List
import Control.Arrow
import Text.Printf
import Control.Monad

hist :: Ord a => [a] -> [a] -> [(a, Int)]
hist xs ys = map (id *** length) (hist' xs (sort ys))
    where
        hist' []     ys = []
        hist' (x:xs) ys = case break (>x) ys of
            (as, bs) -> (x, as) : hist' xs bs

pHist :: (Fractional a, Ord a, PrintfArg a) => Int -> IO a -> IO ()
pHist n x = do
    y <- replicateM n x
    
    let a = minimum y
        b = maximum y
        c = 80
        d = 120
        step = (b - a) / fromInteger c
        steps = [ a + fromInteger n * step
                | n <- [1..c]
                ]
        h = hist steps y
        
        z = maximum (map snd h)
        scale = max 1 (z `div` d)

        fmt (n, x) = printf "%0.3f%9s: " n ( '(' : show x ++ ")" ) ++ replicate (x `div` scale) '*'
        
    mapM_ (putStrLn . fmt) h