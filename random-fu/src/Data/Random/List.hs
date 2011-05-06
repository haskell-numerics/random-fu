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

-- | A random variable that shuffles a list of a known length (or a list
-- prefix of the specified length). Useful for shuffling large lists when 
-- the length is known in advance.  Avoids needing to traverse the list to
-- discover its length.  Each ordering has equal probability.
shuffleN :: Int -> [a] -> RVar [a]
shuffleN n xs = shuffleNofM n n xs

-- | A random variable that selects N arbitrary elements of a list of known length M.
shuffleNofM :: Int -> Int -> [a] -> RVar [a]
shuffleNofM 0 _ _ = return []
shuffleNofM n m xs
    | n > m     = error "shuffleNofM: n > m"
    | n >= 0    = do
        is <- sequence [uniform 0 i | i <- take n [m-1, m-2 ..1]]
        return (take n $ SRS.shuffle (take m xs) is)
shuffleNofM _ _ _ = error "shuffleNofM: negative length specified"

