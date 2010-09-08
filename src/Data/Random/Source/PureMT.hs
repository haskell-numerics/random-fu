{-# LANGUAGE
    BangPatterns,
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances,
    UndecidableInstances,
    GADTs, RankNTypes
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |This module provides functions useful for implementing new 'MonadRandom'
-- and 'RandomSource' instances for state-abstractions containing 'PureMT'
-- values (the pure pseudorandom generator provided by the
-- mersenne-random-pure64 package), as well as instances for some common
-- cases.
-- 
-- A 'PureMT' generator is immutable, so 'PureMT' by itself cannot be a 
-- 'RandomSource' (if it were, it would always give the same \"random\"
-- values).  Some form of mutable state must be used, such as an 'IORef',
-- 'State' monad, etc..  A few default instances are provided by this module
-- along with more-general functions ('getRandomPrimFromMTRef' and
-- 'getRandomPrimFromMTState') usable as implementations for new cases
-- users might need.
module Data.Random.Source.PureMT 
    ( PureMT, newPureMT, pureMT
    , module Data.Random.Source.PureMT 
    ) where

import Data.Random.Internal.Primitives
import Data.Random.Source
import System.Random.Mersenne.Pure64

import Data.StateRef

import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S

-- |Given a function for applying a 'PureMT' transformation to some hidden 
-- state, this function derives a function able to generate all 'Prim's
-- in the given monad.  This is then suitable for either a 'MonadRandom' or
-- 'RandomSource' instance, where the 'supportedPrims' or
-- 'supportedPrimsFrom' function (respectively) is @const True@.
{-# INLINE getRandomPrimBy #-}
getRandomPrimBy :: Monad m => (forall t. (PureMT -> (t, PureMT)) -> m t) -> Prim a -> m a
getRandomPrimBy getThing = getPrimWhere supported (\prim -> getThing (genPrim prim))
    where 
        {-# INLINE supported #-}
        supported :: Prim a -> Bool
        supported PrimWord64 = True
        supported PrimDouble = True
        supported _          = False
        
        {-# INLINE genPrim #-}
        genPrim :: Prim a -> (PureMT -> (a, PureMT))
        genPrim PrimWord64 = randomWord64
        genPrim PrimDouble = randomDouble
        genPrim p = error ("getRandomPrimBy: genPrim called for unsupported prim " ++ show p)

-- |Given a mutable reference to a 'PureMT' generator, we can implement
-- 'RandomSource' for in any monad in which the reference can be modified.
-- 
-- Typically this would be used to define a new 'RandomSource' instance for
-- some new reference type or new monad in which an existing reference type
-- can be modified atomically.  As an example, the following instance could
-- be used to describe how 'IORef' 'PureMT' can be a 'RandomSource' in the
-- 'IO' monad:
-- 
-- > instance RandomSource IO (IORef PureMT) where
-- >     supportedPrimsFrom _ _ = True
-- >     getSupportedRandomPrimFrom = getRandomPrimFromMTRef
-- 
-- (note that there is actually a more general instance declared already
-- covering this as a a special case, so there's no need to repeat this
-- declaration anywhere)
-- 
-- Example usage:
-- 
-- > main = do
-- >     src <- newIORef (pureMT 1234)          -- OR: newPureMT >>= newIORef
-- >     x <- sampleFrom src (uniform 0 100)    -- OR: runRVar (uniform 0 100) src
-- >     print x
getRandomPrimFromMTRef :: (Monad m, ModifyRef sr m PureMT) => sr -> Prim a -> m a
getRandomPrimFromMTRef ref = getRandomPrimBy getThing
    where
        {-# INLINE getThing #-}
        getThing thing = atomicModifyReference ref $ \(!oldMT) -> 
            case thing oldMT of (!w, !newMT) -> (newMT, w)
            

-- |Similarly, @getRandomPrimFromMTState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'PureMT' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables (e.g., by
-- @runState . sample :: Distribution d t => d t -> PureMT -> (t, PureMT)@.
-- 'PureMT' in the type there can be replaced by 'StdGen' or anything else 
-- satisfying @MonadRandom (State s) => s@).
-- 
-- For example, this module includes the following declaration:
-- 
-- > instance MonadRandom (State PureMT) where
-- >     supportedPrims _ _ = True
-- >     getSupportedRandomPrim = getRandomPrimFromMTState
-- 
-- This describes a \"standard\" way of getting random values in 'State'
-- 'PureMT', which can then be used in various ways, for example (assuming 
-- some 'RVar' @foo@ and some 'Word64' @seed@):
-- 
-- > runState (runRVar foo StdRandom) (pureMT seed)
-- > runState (sampleFrom StdRandom foo) (pureMT seed)
-- > runState (sample foo) (pureMT seed)
-- 
-- Of course, the initial 'PureMT' state could also be obtained by any other
-- convenient means, such as 'newPureMT' if you don't care what seed is used.
getRandomPrimFromMTState :: MonadState PureMT m => Prim a -> m a
getRandomPrimFromMTState = getRandomPrimBy getThing
    where
        {-# INLINE getThing #-}
        getThing thing = do
            !mt <- get
            let (!ws, !newMt) = thing mt
            put newMt
            return ws

instance MonadRandom (State PureMT) where
    getRandomPrim = getRandomPrimFromMTState

instance MonadRandom (S.State PureMT) where
    getRandomPrim = getRandomPrimFromMTState

instance (Monad m1, ModifyRef (Ref m2 PureMT) m1 PureMT) => RandomSource m1 (Ref m2 PureMT) where
    getRandomPrimFrom = getRandomPrimFromMTRef
    
instance Monad m => MonadRandom (StateT PureMT m) where
    getRandomPrim = getRandomPrimFromMTState

instance Monad m => MonadRandom (S.StateT PureMT m) where
    getRandomPrim = getRandomPrimFromMTState

instance (Monad m, ModifyRef (IORef PureMT) m PureMT) => RandomSource m (IORef PureMT) where
    {-# SPECIALIZE instance RandomSource IO (IORef PureMT) #-}
    getRandomPrimFrom = getRandomPrimFromMTRef
    
instance (Monad m, ModifyRef (STRef s PureMT) m PureMT) => RandomSource m (STRef s PureMT) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s PureMT) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s PureMT) #-}
    getRandomPrimFrom = getRandomPrimFromMTRef

-- Note that this instance is probably a Bad Idea.  STM allows random variables
-- to interact in spooky quantum-esque ways - One transaction can 'retry' until
-- it gets a \"random\" answer it likes, which causes it to selectively consume 
-- entropy, biasing the supply from which other random variables will draw.
-- instance (Monad m, ModifyRef (TVar PureMT) m PureMT) => RandomSource m (TVar PureMT) where
--     {-# SPECIALIZE instance RandomSource IO  (TVar PureMT) #-}
--     {-# SPECIALIZE instance RandomSource STM (TVar PureMT) #-}
--     getRandomPrimFrom = getRandomPrimFromMTRef
    
