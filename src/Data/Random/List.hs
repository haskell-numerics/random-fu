module Data.Random.List where

import Data.Random.RVar
import Data.Random.Distribution.Uniform

import qualified System.Random.Shuffle as SRS
import Control.Monad

-- | A random variable returning an arbitrary element of the given list.
-- Every element has equal probability of being chosen.  Because it is a
-- pure 'RVar' it has no memory - that is, it \"draws with replacement.\"
randomElement :: [a] -> RVar a
randomElement [] = error "randomElement: empty list!"
randomElement xs = do
    n <- uniform 0 (length xs - 1)
    return (xs !! n)

-- | A random variable that returns the given list in an arbitrary shuffled
-- order.  Every ordering of the list has equal probability.
shuffle :: [a] -> RVar [a]
shuffle [] = return []
shuffle xs = do
    is <- zipWithM (\_ i -> uniform 0 i) (tail xs) [1..]
    
    return (SRS.shuffle xs (reverse is))

-- | A random variable that shuffles a list of a known length. Useful for 
-- shuffling large lists when the length is known in advance.
-- Avoids needing to traverse the list to discover its length.  Each ordering
-- has equal probability.
--
-- Throws an error the list is not exactly as long as claimed.
shuffleN :: Int -> [a] -> RVar [a]
shuffleN 0 xs = return []
shuffleN m@(n+1) xs = do
    is <- sequence [uniform 0 i | i <- [n,n-1..1]]
    return (SRS.shuffle xs is)

    