{-
 -      ``Test/Hist''
 -  A bunch of ad-hoc stuff I use to test sampling functions.
 -}
{-# LANGUAGE
        FlexibleContexts
  #-}

module Test.Hist where

import Prelude hiding (sum)

import Data.Random
import Data.Random.Source
import Data.Random.Source.DevRandom
import System.Random.Mersenne.Pure64

-- some convenient testing stuff
import Data.List hiding (sum)
import Control.Arrow
import Text.Printf
import Control.Monad
import Data.StateRef
import Control.Exception
import Control.Parallel.Strategies
import Control.Monad.Loops
import System.CPUTime
import Control.Concurrent
import GHC.Conc

sum xs = foldl' (+) 0 xs

cpuTimeDiff t0 t1 = fromInteger (t1 - t0) * recip (10^12)

time action = do
    t0 <- liftIO getCPUTime
    result <- action
    t1 <- liftIO getCPUTime
    return (result, cpuTimeDiff t0 t1)

time_ action = do
    t0 <- liftIO getCPUTime
    action
    t1 <- liftIO getCPUTime
    return (cpuTimeDiff t0 t1)

replicateM'chunkSize = 100

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n x
    | n >= replicateM'chunkSize
    = do
        let (chunks, extra) = n `divMod` replicateM'chunkSize
            extraRuns = replicateM extra x
        
        xs <- replicateM' chunks (replicateM replicateM'chunkSize x)
        xtra <- extraRuns
        return (concat (xs ++ [xtra]))
        
    | otherwise = replicateM n x

cpus=numCapabilities -- * 2 - 1

sampleN :: Int -> RVar a -> IO [a]
sampleN n rv
    | cpus == 1     = do
        seed <- getRandomWord64From DevRandom
        mt <- newRef (pureMT seed)    
        replicateM' n (sampleFrom mt rv)
    
    | otherwise = runInUnboundThread $ do
        let (runs, extra) = n `divMod` cpus
            extraRuns = replicateM' extra (sampleFrom DevRandom rv)
    
        sets <- forkMapM id $ (extraRuns :) $ replicate cpus $ do
            seed <- getRandomWord64From DevRandom
            mt <- newRef (pureMT seed)
            replicateM' runs (sampleFrom mt rv)
    
        let extract (Right x) = x
        return (concatMap extract sets)

hist :: Ord a => [a] -> [a] -> [(a, Int)]
hist xs ys = hist' xs (sort ys)
    where
        strictPair a b = a `seq` b `seq` (a,b)
        
        hist' []     ys = []
        hist' (x:xs) ys = case break (>x) ys of
            (as, bs) -> strictPair x (length as) : hist' xs bs

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
    y <- sampleN n x
    putStrLn ("generated " ++ show (length y) ++ " numbers, now making histogram...")
    printHist fmt hist y n

-- cumulative probability histogram
cpHist :: Int -> RVar Double -> IO ()
cpHist n x = do
    y <- sampleN n x
    printHist fmtDbl cHist y n

cpDiff :: CDF d Double => Int -> d Double -> IO ()
cpDiff = cpDiff_ fmtDbl

cpDiff_ :: (CDF d t, Fractional t, Ord t) => (t -> String) -> Int -> d t -> IO ()
cpDiff_ fmt n x = do
    mt <- newPureMT
    mt <- newRef mt
    y <- sampleN n (rvar x)
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
            getRandomWord8From src
    return (src', dx) `asTypeOf` (undefined :: IO (m word8, m int))

uniformize :: CDF d t => d t -> RVar Double
uniformize dist = fmap (cdf dist) (rvar dist)
