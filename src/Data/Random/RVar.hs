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


import Data.Random.Internal.Primitives
import Data.Random.Source
import Data.Random.Source.Std
import Data.Random.Lift as L

import Data.Bits

import qualified Control.Monad.Trans as T
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Prompt (PromptT, runPromptT, prompt)

-- |An opaque type containing a \"random variable\" - a value 
-- which depends on the outcome of some random process.
type RVar = RVarT Identity

runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar = runRVarT

-- |A random variable with access to operations in an underlying monad.  Useful
-- examples include any form of state for implementing random processes with hysteresis,
-- or writer monads for implementing tracing of complicated algorithms.
newtype RVarT m a = RVarT { unRVarT :: PromptT Prim m a }

-- | \"Runs\" the monad.
{-# INLINE runRVarT #-}
runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT (RVarT m) src = runPromptT return bindP bindN m
    where
        bindP prim cont = getRandomPrimFrom src prim >>= cont
        bindN nExp cont = lift nExp >>= cont

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return x)
    fail s   = RVarT (fail s)
    (RVarT m) >>= k = RVarT (m >>= unRVarT.k)

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance T.MonadTrans RVarT where
    lift m = RVarT (T.lift m)

instance Lift (RVarT Identity) (RVarT m) where
    lift (RVarT m) = RVarT (runPromptT return bindP bindN m)
        where
            bindP prim  cont = prompt prim >>= cont
            bindN idExp cont = cont (runIdentity idExp)

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

instance MonadRandom (RVarT n) where
    supportedPrims _ p  = True
    getRandomPrim p     = RVarT (prompt p)
    getRandomByte       = RVarT (prompt PrimWord8)
    getRandomWord       = RVarT (prompt PrimWord64)
    getRandomDouble     = RVarT (prompt PrimDouble)

-- I would really like to be able to do this, but I can't because of the
-- blasted Eq and Show in Num's class context...
-- instance (Applicative m, Num a) => Num (RVarT m a) where
--     (+) = liftA2 (+)
--     (-) = liftA2 (-)
--     (*) = liftA2 (*)
--     negate = liftA negate
--     signum = liftA signum
--     abs = liftA abs
--     fromInteger = pure . fromInteger

-- some 'fundamental' RVarTs
-- this maybe ought to even be a part of the RandomSource class...
{-# INLINE nByteInteger #-}
-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^(8*n)-1, inclusive.
nByteInteger :: Int -> RVarT m Integer
nByteInteger 1 = do
    x <- getRandomByte
    return $! toInteger x
nByteInteger 8 = do
    x <- getRandomWord
    return $! toInteger x
nByteInteger n = nBitInteger (n `shiftL` 3)

{-# INLINE nBitInteger #-}
-- |A random variable evenly distributed over all unsigned integers from
-- 0 to 2^n-1, inclusive.
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
