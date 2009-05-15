{-
 -      ``Data/Random/RVar''
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances, 
    GADTs
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


import Data.Random.Internal.Words
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

-- | single combined container allowing all the relevant 
-- dictionaries (plus the RandomSource item itself) to be passed
-- with one pointer.
data RVarDict n m where
    RVarDict :: (Lift n m, Monad m, RandomSource m s) => s -> RVarDict n m

runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar = runRVarT

-- |An opaque type containing a \"random variable\" - a value 
-- which depends on the outcome of some random process.
newtype RVarT n a = RVarT { unRVarT :: forall m. ReaderT (RVarDict n m) m a }

-- | \"Runs\" the monad.
runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT (RVarT m) (src) = runReaderT m (RVarDict src)

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (ReaderT (\(RVarDict _) -> return x))
    fail s   = RVarT (ReaderT (\(RVarDict _) -> fail s))
    (RVarT x) >>= f = RVarT (ReaderT (\d@(RVarDict _) -> 
        runReaderT x d >>= (\x -> runReaderT (unRVarT (f x)) d)))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance T.MonadTrans RVarT where
    lift m = RVarT (ReaderT (\(RVarDict _) -> L.lift m))

instance Lift (RVarT Identity) (RVarT m) where
    lift (RVarT m) = RVarT (ReaderT (\(RVarDict src) -> runReaderT m (RVarDict src)))

instance MonadRandom (RVarT n) where
    getRandomByte = RVarT (ReaderT $ \(RVarDict src) -> getRandomByteFrom src)
    getRandomWord = RVarT (ReaderT $ \(RVarDict src) -> getRandomWordFrom src)

-- some 'fundamental' RVarTs
-- this maybe ought to even be a part of the RandomSource class...
-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^(8*n)-1, inclusive.
{-# INLINE nByteInteger #-}
nByteInteger :: Int -> RVarT m Integer
nByteInteger 1 = do
    x <- getRandomByte
    return $! toInteger x
nByteInteger 8 = do
    x <- getRandomWord
    return $! toInteger x
nByteInteger (n+8) = do
    x <- getRandomWord
    y <- nByteInteger n
    return $! ((toInteger x `shiftL` n) .|. y)
nByteInteger n = do
    x <- getRandomWord
    return $! toInteger (x `shiftR` ((8-n) `shiftL` 3))

-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^n-1, inclusive.
{-# INLINE nBitInteger #-}
nBitInteger :: Int -> RVarT m Integer
nBitInteger 8  = do
    x <- getRandomByte
    return $! toInteger x
nBitInteger (n+64) = do
    x <- getRandomWord
    y <- nBitInteger n
    return $! (toInteger x `shiftL` n) .|. y
nBitInteger n = do
        x <- getRandomWord
        return $! toInteger (x `shiftR` (64-n))
