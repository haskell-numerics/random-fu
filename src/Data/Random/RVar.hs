{-
 -      ``Data/Random/RVar''
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances
  #-}

-- |Random variables.  An 'RVar' is a sampleable random variable.  Because
-- probability distributions form a monad, they are quite easy to work with
-- in the standard Haskell monadic styles.  For examples, see the source for
-- any of the 'Distribution' instances - they all are defined in terms of
-- 'RVar's.
module Data.Random.RVar
    ( RVar
    
    , nByteInteger
    , nBitInteger
    ) where

import Data.Random.Distribution
import Data.Random.Source

import Data.Word
import Data.Bits

import Control.Applicative
import Control.Monad

-- |An opaque type containing a \"random variable\" - a value 
-- which depends on the outcome of some random process.
newtype RVar a = RVar { runRVar :: forall m s. RandomSource m s => s -> m a }

instance Functor RVar where
    fmap = liftM

instance Monad RVar where
    return x = RVar (\_ -> return x)
    fail s   = RVar (\_ -> fail s)
    (RVar x) >>= f = RVar (\s -> do
            x <- x s
            runRVar (f x) s
        )

instance Applicative RVar where
    pure  = return
    (<*>) = ap

instance Distribution RVar a where
    rvar = id
    sampleFrom src x = runRVar x src

instance MonadRandom RVar where
    getRandomBytes n = RVar (\s -> getRandomBytesFrom s n)
    getRandomWords n = RVar (\s -> getRandomWordsFrom s n)

-- some 'fundamental' RVars
-- this maybe ought to even be a part of the RandomSource class...
-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^(8*n)-1, inclusive.
nByteInteger :: Int -> RVar Integer
nByteInteger n
    | n .&. 7 == 0
    = do
        xs <- getRandomWords (n `shiftR` 3)
        return $! concatWords xs
    | n > 8
    = do
        let nWords = n `shiftR` 3
            nBytes = n .&. 7
        ws <- getRandomWords nWords
        bs <- getRandomBytes nBytes
        return $! ((concatWords ws `shiftL` (nBytes `shiftL` 3)) .|. concatBytes bs)
    | otherwise
    = do
        xs <- getRandomBytes n
        return $! concatBytes xs

-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^n-1, inclusive.
nBitInteger :: Int -> RVar Integer
nBitInteger n
    | n .&. 7 == 0
    = nByteInteger (n `shiftR` 3)
    | otherwise
    = do
        x <- nByteInteger ((n `shiftR` 3) + 1)
        return $! (x .&. (bit n - 1))

concatBytes :: (Bits a, Num a) => [Word8] -> a
concatBytes = concatBits fromIntegral

concatWords :: (Bits a, Num a) => [Word64] -> a
concatWords = concatBits fromIntegral

concatBits :: (Bits a, Bits b, Num b) => (a -> b) -> [a] -> b
concatBits f [] = 0
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)
