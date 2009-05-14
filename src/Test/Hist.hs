{-
 -      ``Test/Hist''
 -}

module Test.Hist where

import Data.Random

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

-- cumulative histogram
cHist xs ys = tail (scanl (\(_, p1) (x,p2) -> (x, p1+p2)) (undefined, 0) (hist xs ys))

-- probability density histogram
pHist :: Int -> RVar Double -> IO ()
pHist = pHist_

pHist_ :: (Floating a, Ord a, PrintfArg a) => Int -> RVar a -> IO ()
pHist_ n x = do
    y <- replicateM n (sampleFrom DevRandom x)
    printHist hist y n

-- cumulative probability histogram
cpHist :: Int -> RVar Double -> IO ()
cpHist n x = do
    y <- replicateM n (sampleFrom DevRandom x)
    printHist cHist y n

-- byte-count histogram (random source usage)
-- bcHist :: Int -> RVar Double -> IO ()
-- bcHist n x = do
--     (src, dx) <- mkByteCounter DevRandom
--     y <- replicateM n (sampleFrom src x >> fmap fromIntegral dx) :: IO [Double]
--     printHist hist y n

printHist hist ys n = mapM_ (putStrLn . fmt) xs
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
        
        maxX = maximum (map snd xs)
        scale = fromIntegral maxX / cols
        
        fmt (bin, x) = printf "%+0.3f%9s: " bin (printf "(%0.2f%%)" (100 * fromIntegral x / fromIntegral n :: Float) :: String) ++ replicate (round (fromIntegral x / scale)) '*'

-- mkByteCounter src = do
--     x <- newDefaultRef 0
--     dx <- mkLapseReader x (-)
--     let src' i = do
--             modifyRef x (+i)
--             getRandomBytesFrom src i
--     return (src', dx) `asTypeOf` (undefined :: m (int -> m (vector word8), m int))

