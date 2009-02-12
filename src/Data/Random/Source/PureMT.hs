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
getRandomWordsFromMTRef :: ModifyRef sr m PureMT => sr -> Int -> m [Word64]
getRandomWordsFromMTRef ref n = do
    atomicModifyRef ref (randomWords n [])
    
    where
        swap (a,b) = (b,a)
        randomWords    0  ws mt = (mt, ws)
        randomWords (n+1) ws mt = case randomWord64 mt of
            (w, mt) -> randomWords n (w:ws) mt

-- |Similarly, @getRandomWordsFromMTState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'PureMT' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
getRandomWordsFromMTState :: MonadState PureMT m => Int -> m [Word64]
getRandomWordsFromMTState n = do
    mt <- get
    let randomWords    0  ws mt = (mt, ws)
        randomWords (n+1) ws mt = case randomWord64 mt of
            (w, mt) -> randomWords n (w:ws) mt
        
        (newMt, ws) = randomWords n [] mt
    put newMt
    return ws

instance MonadRandom (State PureMT) where
    getRandomWords = getRandomWordsFromMTState

instance Monad m => MonadRandom (StateT PureMT m) where
    getRandomWords = getRandomWordsFromMTState
