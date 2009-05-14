{-
 -      ``Data/Random/Source''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , RandomSource(..)
    ) where

import Data.Word
import Data.Bits
import Data.List
import Control.Monad

import Data.Random.Internal.Words

-- |A typeclass for monads with a chosen source of entropy.  For example,
-- 'RVar' is such a monad - the source from which it is (eventually) sampled
-- is the only source from which a random variable is permitted to draw, so
-- when directly requesting entropy for a random variable these functions
-- are used.
-- 
-- The minimal definition is either 'getRandomBytes' or 'getRandomWords'.
class Monad m => MonadRandom m where
    getRandomByte :: m Word8
    getRandomByte = do
        word <- getRandomWord
        return (fromIntegral word)
    
    getRandomWord :: m Word64
    getRandomWord = do
        b0 <- getRandomByte
        b1 <- getRandomByte
        b2 <- getRandomByte
        b3 <- getRandomByte
        b4 <- getRandomByte
        b5 <- getRandomByte
        b6 <- getRandomByte
        b7 <- getRandomByte
        
        return (buildWord b0 b1 b2 b3 b4 b5 b6 b7)

-- |A source of entropy which can be used in the given monad.
--
-- The minimal definition is either 'getRandomBytesFrom' or 'getRandomWordsFrom'
class Monad m => RandomSource m s where
    -- TODO: make defaulting situation more comprehensible
    getRandomByteFrom :: s -> m Word8
    getRandomByteFrom src = do
        word <- getRandomWordFrom src
        return (fromIntegral word)
    
    getRandomWordFrom :: s -> m Word64
    getRandomWordFrom src = do
        b0 <- getRandomByteFrom src
        b1 <- getRandomByteFrom src
        b2 <- getRandomByteFrom src
        b3 <- getRandomByteFrom src
        b4 <- getRandomByteFrom src
        b5 <- getRandomByteFrom src
        b6 <- getRandomByteFrom src
        b7 <- getRandomByteFrom src
        
        return (buildWord b0 b1 b2 b3 b4 b5 b6 b7)

instance Monad m => RandomSource m (m Word8) where
    getRandomByteFrom = id

instance Monad m => RandomSource m (m Word64) where
    getRandomWordFrom = id
