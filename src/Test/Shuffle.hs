{-
 -      ``Test/Shuffle''
 -}
{-# LANGUAGE
    FlexibleContexts
  #-}

module Test.Shuffle where

import Data.Random
import GHC.IOBase

shuffleFrom src list = shuffle (length list) list
    where
        shuffle 0 [] = return []
        shuffle (n+1) l = do
            i <- sampleFrom src (Uniform 0 n)
            let (x, xs) = extract i l
            ys <- shuffle n xs
            return (x:ys)
        
        extract 0 (x:xs) = (x, xs)
        extract (n+1) (x:xs) = (y, x:ys)
            where
                (y,ys) = extract n xs

lazyShuffleFrom :: (RandomSource IO s) =>
                   s -> Integer -> [a] -> IO [a]
lazyShuffleFrom src = shuffle
    where
        shuffle 0 _  = return []
        shuffle _ [] = return []
        shuffle (n+1) l = do
            i <- sampleFrom src (Uniform 0 n)
            let (x, xs) = extract i l
            ys <- unsafeInterleaveIO (shuffle n xs)
            return (x:ys)
        
        extract 0 (x:xs) = (x, xs)
        extract (n+1) (x:xs) = (y, x:ys)
            where
                (y,ys) = extract n xs

