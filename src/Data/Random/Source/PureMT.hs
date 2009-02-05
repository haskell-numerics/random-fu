{-
 -      ``Data/Random/Source/PureMT''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
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

getRandomWordsFromMTRef :: ModifyRef sr m PureMT => sr -> Int -> m [Word64]
getRandomWordsFromMTRef ref n = do
    atomicModifyRef ref (randomWords n [])
    
    where
        swap (a,b) = (b,a)
        randomWords    0  ws mt = (mt, ws)
        randomWords (n+1) ws mt = case randomWord64 mt of
            (w, mt) -> randomWords n (w:ws) mt

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
