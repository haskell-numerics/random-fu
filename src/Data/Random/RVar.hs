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
    , runRVar
    , RVarT
    , runRVarT
    , nByteInteger
    , nBitInteger
    ) where


import Data.Random.Source
import Data.Random.Lift as L

import Data.Word
import Data.Bits

import qualified Control.Monad.Trans as T
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity

type RVar = RVarT Identity

runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar = runRVarT

-- |An opaque type containing a \"random variable\" - a value 
-- which depends on the outcome of some random process.
newtype RVarT n a = RVarT { unRVarT :: forall m s. (Lift n m, RandomSource m s) => ReaderT s m a }

-- | \"Runs\" the monad.
runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT = runReaderT . unRVarT

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return x)
    fail s   = RVarT (fail s)
    (RVarT x) >>= f = RVarT (x >>= unRVarT . f)

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance T.MonadTrans RVarT where
    lift m = RVarT (T.lift . L.lift $ m)


instance MonadRandom (RVarT n) where
    getRandomBytes n = RVarT (ReaderT $ \s -> getRandomBytesFrom s n)
    getRandomWords n = RVarT (ReaderT $ \s -> getRandomWordsFrom s n)

-- some 'fundamental' RVarTs
-- this maybe ought to even be a part of the RandomSource class...
-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^(8*n)-1, inclusive.
nByteInteger :: Int -> RVarT m Integer
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
nBitInteger :: Int -> RVarT m Integer
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
