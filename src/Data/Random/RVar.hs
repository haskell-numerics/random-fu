{-
 -      ``Data/Random/RVar''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances
  #-}

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
import Control.Monad.State

newtype RVar a = RVar { runDistM :: forall m s. RandomSource m s => s -> m a }

instance Functor RVar where
    fmap = liftM

instance Monad RVar where
    return x = RVar (\_ -> return x)
    fail s   = RVar (\_ -> fail s)
    (RVar x) >>= f = RVar (\s -> do
            x <- x s
            case f x of
                RVar y -> y s
        )

instance Applicative RVar where
    pure  = return
    (<*>) = ap

instance Distribution RVar a where
    rvar = id
    sampleFrom src x = runDistM x src

instance MonadRandom RVar where
    getRandomBytes n = RVar (\s -> getRandomBytesFrom s n)

-- some 'fundamental' RVars
nByteInteger :: Int -> RVar Integer
nByteInteger n = do
    xs <- getRandomBytes n
    return (concatBytes xs)

nBitInteger :: Int -> RVar Integer
nBitInteger n
    | n .&. 7 == 0
    = nByteInteger (n `shiftR` 3)
    | otherwise
    = do
    x <- nByteInteger ((n `shiftR` 3) + 1)
    return (x .&. (bit n - 1))

concatBytes :: (Bits a, Num a) => [Word8] -> a
concatBytes = concatBits fromIntegral

concatBits :: (Bits a, Bits b, Num b) => (a -> b) -> [a] -> b
concatBits f [] = 0
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)
