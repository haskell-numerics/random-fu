{-
 -      ``Data/Random/Source/PureMT''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances
  #-}

module Data.Random.Source.PureMT where

import Data.Random.Source
import System.Random.Mersenne.Pure64

import Data.StateRef
import Data.Word

import Control.Monad.State

-- |Given a mutable reference to a 'PureMT' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
--
-- For example, if @x :: TVar PureMT@, @getRandomWordsFromMTRef x@ can be
-- used as a 'RandomSource' in 'IO', 'STM', or any monad which is an instance
-- of 'MonadIO'.
getRandomWordFromMTRef :: ModifyRef sr m PureMT => sr -> m Word64
getRandomWordFromMTRef ref = do
    atomicModifyRef ref (swap . randomWord64)
    
    where
        swap (a,b) = (b,a)

getRandomByteFromMTRef :: ModifyRef sr m PureMT => sr -> m Word8
getRandomByteFromMTRef ref = do
    x <- atomicModifyRef ref (swap . randomInt)
    return (fromIntegral x)
    
    where
        swap (a,b) = (b,a)

-- |Similarly, @getRandomWordsFromMTState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'PureMT' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
getRandomWordFromMTState :: MonadState PureMT m => m Word64
getRandomWordFromMTState = do
    mt <- get
    let (ws, newMt) = randomWord64 mt
    put newMt
    return ws

getRandomByteFromMTState :: MonadState PureMT m => m Word8
getRandomByteFromMTState = do
    mt <- get
    let (ws, newMt) = randomInt mt
    put newMt
    return (fromIntegral ws)

instance MonadRandom (State PureMT) where
    getRandomByte  = getRandomByteFromMTState
    getRandomWord  = getRandomWordFromMTState

instance Monad m => MonadRandom (StateT PureMT m) where
    getRandomByte  = getRandomByteFromMTState
    getRandomWord  = getRandomWordFromMTState

instance RandomSource IO (IORef PureMT) where
    getRandomByteFrom  = getRandomByteFromMTRef
    getRandomWordFrom  = getRandomWordFromMTRef

instance RandomSource (ST s) (STRef s PureMT) where
    getRandomByteFrom  = getRandomByteFromMTRef
    getRandomWordFrom  = getRandomWordFromMTRef

