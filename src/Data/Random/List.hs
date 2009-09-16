module Data.Random.List where

import Data.Random.RVar
import Data.Random.Distribution.Uniform

import qualified System.Random.Shuffle as SRS
import Control.Monad

randomElement :: [a] -> RVar a
randomElement [] = error "randomElement: empty list!"
randomElement xs = do
    n <- uniform 0 (length xs - 1)
    return (xs !! n)

shuffle :: [a] -> RVar [a]
shuffle [] = return []
shuffle xs = do
    is <- zipWithM (\_ i -> uniform 0 i) (tail xs) [1..]
    
    return (SRS.shuffle xs (reverse is))

shuffleN :: Int -> [a] -> RVar [a]
shuffleN 0 xs = return []
shuffleN (n+1) xs = do
    is <- sequence [uniform 0 i | i <- [n,n-1..1]]
    return (SRS.shuffle xs is)
    