{-
 -      ``Test/Hist''
 -  A bunch of ad-hoc stuff I use to test sampling functions.
 -}
{-# LANGUAGE
        FlexibleContexts
  #-}

module Test.Hist where

import Data.Random
import System.Random.Mersenne.Pure64

-- some convenient testing stuff
import Data.List
import Control.Arrow
import Text.Printf
import Control.Monad
import Data.StateRef
import Control.Exception

hist :: Ord a => [a] -> [a] -> [(a, Int)]
hist xs ys = map (id *** length) (hist' xs (sort ys))
    where
        hist' []     ys = []
        hist' (x:xs) ys = case break (>x) ys of
            (as, bs) -> (x, as) : hist' xs bs

-- cumulative histogram
cHist xs ys = tail (scanl (\(_, p1) (x,p2) -> (x, p1+p2)) (undefined, 0) (hist xs ys))

-- cumulative diff histogram
cDiff n cdf xs ys =
     [ (y, toCount (p - cdf y))
     | (y, count) <- cHist xs ys
     , let p = fromIntegral count / fromIntegral n
     ]
     
     where
         toCount p = round (p * fromIntegral n)

-- probability density histogram
pHist :: Int -> RVar Double -> IO ()
pHist = pHist_ fmtDbl

pHist_ :: (Fractional a, Ord a) => (a -> String) -> Int -> RVar a -> IO ()
pHist_ fmt n x = do
    y <- replicateM n (sampleFrom DevRandom x)
    printHist fmt hist y n

-- cumulative probability histogram
cpHist :: Int -> RVar Double -> IO ()
cpHist n x = do
    y <- replicateM n (sampleFrom DevRandom x)
    printHist fmtDbl cHist y n

cpDiff :: CDF d Double => Int -> d Double -> IO ()
cpDiff = cpDiff_ fmtDbl

cpDiff_ :: (CDF d t, Fractional t, Ord t) => (t -> String) -> Int -> d t -> IO ()
cpDiff_ fmt n x = do
    mt <- newPureMT
    mt <- newRef mt
    y <- replicateM n (sampleFrom mt x)
    printHist fmt (cDiff n (cdf x)) y n

--byte-count histogram (random source usage)
bcHist :: Int -> RVar Double -> IO ()
bcHist n x = do
    (src, dx) <- mkByteCounter DevRandom
    y <- replicateM n (sampleFrom src x >> fmap fromIntegral dx) :: IO [Double]
    printHist fmtDbl hist y n

fmtDbl :: Double -> String
fmtDbl = printf "%+0.3f"

printHist :: (Integral a2, Fractional a, Ord a) =>
             (a1 -> String) ->
             ([a] -> [a] -> [(a1, Int)]) -> [a] -> a2 -> IO ()
printHist fmtBin hist ys n = do
    mapM_ (putStrLn . fmt) xs
    printf "Sum: %d (%f%%)\n" (sum (map (snd) xs)) (sum ( pcts))
    printf "Abs Sum: %d (%f%%)\n" (sum (map (abs.snd) xs)) (sum (map abs pcts))
    printf "RMS: (%f%%)\n\n" (sqrt (sum (map ((*100).(^2).frac.snd) xs) / fromIntegral n) :: Double)
    where
        y0 = minimum ys
        y1 = maximum ys
        rows = 80
        cols = 140
        step = (y1 - y0) / fromInteger rows
        steps = [ y0 + fromInteger n * step
                | n <- [1..rows]
                ]
        xs = hist steps ys
        
        maxX = maximum (map (abs.snd) xs)
        scale = fromIntegral maxX / cols
        
        frac x = fromIntegral x / fromIntegral n
        pct x = 100 * frac x
        pcts = map (pct.snd) xs :: [Float]
        fmt (bin, x) = printf "%9s%9s: " (fmtBin bin) (printf "(%0.2f%%)" (pct x :: Float) :: String) ++ replicate (abs (round (fromIntegral x / scale))) '*'

mkByteCounter src = do
    x <- newRef 0
    dx <- mkLapseReader x (-)
    let src' = do
            modifyRef x succ
            readRef x >>= evaluate
            getRandomByteFrom src
    return (src', dx) `asTypeOf` (undefined :: IO (m word8, m int))

uniformize :: CDF d t => d t -> RVar Double
uniformize dist = fmap (cdf dist) (rvar dist)
