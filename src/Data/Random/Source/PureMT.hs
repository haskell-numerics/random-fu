{-# LANGUAGE
    BangPatterns,
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances,
    UndecidableInstances,
    GADTs
  #-}

-- |This module provides functions useful for implementing new 'MonadRandom'
-- and 'RandomSource' instances for state-abstractions containing 'PureMT'
-- values (the pure pseudorandom generator provided by the
-- mersenne-random-pure64 package), as well as instances for some common
-- cases.
module Data.Random.Source.PureMT where

import Data.Random.Internal.Words
import Data.Random.Internal.Primitives
import Data.Random.Source
import System.Random.Mersenne.Pure64

import Data.StateRef
import Data.Word

import Control.Monad.Prompt
import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S

-- |Given a mutable reference to a 'PureMT' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
getRandomPrimFromMTRef :: (Monad m, ModifyRef sr m PureMT) => sr -> Prim a -> m a
getRandomPrimFromMTRef ref PrimWord64 = do
    atomicModifyReference ref $ \(!mt) -> case randomWord64 mt of
        (!w, !mt) -> (mt, w)
-- for whatever reason, my simple wordToDouble is faster than whatever
-- the mersenne random library is using, at least in the version I have.
-- if this changes, switch to the commented version.
-- Same thing below, in getRandomDoubleFromMTState.
getRandomPrimFromMTRef ref PrimDouble = liftM wordToDouble (getRandomPrimFromMTRef ref PrimWord64)
-- getRandomPrimFromMTRef ref PrimDouble = do
--    atomicModifyReference ref $ \(!mt) -> case randomDouble mt of
--        (!w, !mt) -> (mt, w)
getRandomPrimFromMTRef ref other = runPromptM (getRandomPrimFromMTRef ref) (decomposePrimWhere supported other)
    where 
        supported :: Prim a -> Bool
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False

-- |Similarly, @getRandomPrimFromMTState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'PureMT' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables (e.g., by
-- @runState . sample :: Distribution d t => d t -> PureMT -> (t, PureMT)@.
-- 'PureMT' in the type there can be replaced by 'StdGen' or anything else 
-- satisfying @MonadRandom (State s) => s@).
getRandomPrimFromMTState :: MonadState PureMT m => Prim a -> m a
getRandomPrimFromMTState PrimWord64 = do
    !mt <- get
    let (!ws, !newMt) = randomWord64 mt
    put newMt
    return ws
getRandomPrimFromMTState PrimDouble = liftM wordToDouble (getRandomPrimFromMTState PrimWord64)
-- getRandomPrimFromMTState PrimDouble = do
--     !mt <- get
--     let (!x, !newMt) = randomDouble mt
--     put newMt
--     return x
getRandomPrimFromMTState other = runPromptM getRandomPrimFromMTState (decomposePrimWhere supported other)
    where 
        supported :: Prim a -> Bool
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False

instance MonadRandom (State PureMT) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromMTState

instance MonadRandom (S.State PureMT) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromMTState

instance (Monad m1, ModifyRef (Ref m2 PureMT) m1 PureMT) => RandomSource m1 (Ref m2 PureMT) where
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromMTRef
    
instance Monad m => MonadRandom (StateT PureMT m) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromMTState

instance Monad m => MonadRandom (S.StateT PureMT m) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = getRandomPrimFromMTState

instance (Monad m, ModifyRef (IORef PureMT) m PureMT) => RandomSource m (IORef PureMT) where
    {-# SPECIALIZE instance RandomSource IO (IORef PureMT)#-}
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromMTRef
    
instance (Monad m, ModifyRef (STRef s PureMT) m PureMT) => RandomSource m (STRef s PureMT) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s PureMT) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s PureMT) #-}
    supportedPrimsFrom _ _ = True
    getSupportedRandomPrimFrom = getRandomPrimFromMTRef

-- Note that this instance is probably a Bad Idea.  STM allows random variables
-- to interact in spooky quantum-esque ways - One transaction can 'retry' until
-- it gets a \"random\" answer it likes, which causes it to selectively consume 
-- entropy, biasing the supply from which other random variables will draw.
-- instance (Monad m, ModifyRef (TVar PureMT) m PureMT) => RandomSource m (TVar PureMT) where
--     {-# SPECIALIZE instance RandomSource IO  (TVar PureMT) #-}
--     {-# SPECIALIZE instance RandomSource STM (TVar PureMT) #-}
--     supportedPrimsFrom _ _ = True
--     getSupportedRandomPrimFrom = getRandomPrimFromMTRef
    
