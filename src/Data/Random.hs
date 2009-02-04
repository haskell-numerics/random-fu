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
    , module Data.Random.Distribution.Bernoulli
    , module Data.Random.Distribution.Beta
    , module Data.Random.Distribution.Binomial
    , module Data.Random.Distribution.Gamma
    , module Data.Random.Distribution.Exponential
    , module Data.Random.Distribution.Normal
    , module Data.Random.Distribution.Poisson
    , module Data.Random.Distribution.Uniform
    , module Data.Random.RVar
    ) where

import Data.Random.Source
import Data.Random.Source.DevRandom
import Data.Random.Source.Old
import Data.Random.Source.PreFetch
import Data.Random.Source.Std
import Data.Random.Distribution
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Uniform
import Data.Random.RVar

-- some convenient testing stuff
import Data.List
import Control.Arrow
import Text.Printf
import Control.Monad
import Data.StateRef

hist :: Ord a => [a] -> [a] -> [(a, Int)]
hist xs ys = map (id *** length) (hist' xs (sort ys))
    where
        hist' []     ys = []
        hist' (x:xs) ys = case break (>x) ys of
            (as, bs) -> (x, as) : hist' xs bs

-- probability density histogram
pHist :: Int -> RVar Double -> IO ()
pHist n x = do
    y <- replicateM n (sampleFrom DevRandom x)
    printHist y n

-- byte-count histogram (random source usage)
bcHist :: Int -> RVar Double -> IO ()
bcHist n x = do
    (src, dx) <- mkByteCounter DevRandom
    y <- replicateM n (sampleFrom src x >> fmap fromIntegral dx) :: IO [Double]
    printHist y n

printHist y n = mapM_ (putStrLn . fmt) h
    where
        a = minimum y
        b = maximum y
        rows = 80
        cols = 140
        step = (b - a) / fromInteger rows
        steps = [ a + fromInteger n * step
                | n <- [1..rows]
                ]
        h = hist steps y
        
        maxVal = maximum (map snd h)
        scale = fromIntegral maxVal / cols
        
        fmt (bin, x) = printf "%+0.3f%9s: " bin (printf "(%0.2f%%)" (100 * fromIntegral x / fromIntegral n :: Float) :: String) ++ replicate (round (fromIntegral x / scale)) '*'

-- cumulative density histogram
cHist xs ys = scanl1 (+) (map snd $ hist xs ys)

mkByteCounter src = do
    x <- newDefaultRef 0
    dx <- mkLapseReader x (-)
    let src' i = do
            modifyRef x (+i)
            getRandomBytesFrom src i
    return (src', dx) `asTypeOf` (undefined :: m (int -> m [word8], m int))

