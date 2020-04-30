{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs,
    BangPatterns, RankNTypes,
    ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |This module provides functions useful for implementing new 'MonadRandom'
-- and 'RandomSource' instances for state-abstractions containing 'StdGen'
-- values (the pure pseudorandom generator provided by the System.Random
-- module in the \"random\" package), as well as instances for some common
-- cases.
module Data.Random.Source.StdGen
    ( StdGen
    , mkStdGen
    , newStdGen

    , getRandomPrimFromStdGenIO
    , RandomGen.getRandomPrimFromRandomGenRef
    , RandomGen.getRandomPrimFromRandomGenState
    ) where

import Data.Random.Internal.Source
import qualified Data.Random.Source.RandomGen as RandomGen
import System.Random
import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import Data.StateRef


-- Note that this instance is probably a Bad Idea.  STM allows random variables
-- to interact in spooky quantum-esque ways - One transaction can 'retry' until
-- it gets a \"random\" answer it likes, which causes it to selectively consume
-- entropy, biasing the supply from which other random variables will draw.
-- instance (Monad m, ModifyRef (TVar    StdGen) m StdGen) => RandomSource m (TVar    StdGen) where
--     {-# SPECIALIZE instance RandomSource IO  (TVar StdGen) #-}
--     {-# SPECIALIZE instance RandomSource STM (TVar StdGen) #-}
--     supportedPrimsFrom _ _ = True
--     getSupportedRandomPrimFrom = getRandomPrimFromRandomGenRef

instance (Monad m, ModifyRef (STRef s StdGen) m StdGen) => RandomSource m (STRef s StdGen) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s StdGen) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s StdGen) #-}
    getRandomPrimFrom = RandomGen.getRandomPrimFromRandomGenRef

getRandomPrimFromStdGenIO :: Prim a -> IO a
getRandomPrimFromStdGenIO
    = getStdRandom
    . runState
    . getRandomPrim

-- |Similarly, @getRandomWordFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
--
-- Again, see "Data.Random.Source.PureMT".'getRandomPrimFromMTState' for more
-- detailed usage hints - this function serves exactly the same purpose except
-- for a 'StdGen' generator instead of a 'PureMT' generator.
{-# SPECIALIZE RandomGen.getRandomPrimFromRandomGenState :: Prim a -> State StdGen a #-}
{-# SPECIALIZE RandomGen.getRandomPrimFromRandomGenState :: Monad m => Prim a -> StateT StdGen m a #-}
