{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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
-- along with a more-general function ('getRandomPrimFromMTRef') usable as
-- an implementation for new cases users might need.
module Data.Random.Source.PureMT 
    ( PureMT, newPureMT, pureMT
    
    , getRandomPrimFromMTRef
    ) where

import Control.Monad.State
import qualified Control.Monad.State.Strict as S
import Data.Random.Internal.Source
import Data.Random.Source.Internal.TH
import Data.StateRef
import System.Random.Mersenne.Pure64

{-# INLINE withMTRef #-}
withMTRef :: (Monad m, ModifyRef sr m PureMT) => (PureMT -> (t, PureMT)) -> sr -> m t
withMTRef thing ref = atomicModifyReference ref $ \(!oldMT) -> 
    case thing oldMT of (!w, !newMT) -> (newMT, w)

{-# INLINE withMTState #-}
withMTState :: MonadState PureMT m => (PureMT -> (t, PureMT)) -> m t
withMTState thing = do
    !mt <- get
    let (!ws, !newMt) = thing mt
    put newMt
    return ws

#ifndef MTL2

$(monadRandom
    [d| instance MonadRandom (State PureMT) where
            getRandomWord64 = withMTState randomWord64
            getRandomDouble = withMTState randomDouble
     |])

$(monadRandom
    [d| instance MonadRandom (S.State PureMT) where
            getRandomWord64 = withMTState randomWord64
            getRandomDouble = withMTState randomDouble
     |])

#endif

$(randomSource
    [d| instance (Monad m1, ModifyRef (Ref m2 PureMT) m1 PureMT) => RandomSource m1 (Ref m2 PureMT) where
            getRandomWord64From = withMTRef randomWord64
            getRandomDoubleFrom = withMTRef randomDouble
    |])

$(monadRandom
    [d| instance Monad m => MonadRandom (StateT PureMT m) where
            getRandomWord64 = withMTState randomWord64
            getRandomDouble = withMTState randomDouble
     |])

$(monadRandom
    [d| instance Monad m => MonadRandom (S.StateT PureMT m) where
            getRandomWord64 = withMTState randomWord64
            getRandomDouble = withMTState randomDouble
     |])

$(randomSource
    [d| instance (MonadIO m) => RandomSource m (IORef PureMT) where
            getRandomWord64From = withMTRef randomWord64
            getRandomDoubleFrom = withMTRef randomDouble
     |])

$(randomSource
    [d| instance (Monad m, ModifyRef (STRef s PureMT) m PureMT) => RandomSource m (STRef s PureMT) where
            getRandomWord64From = withMTRef randomWord64
            getRandomDoubleFrom = withMTRef randomDouble
     |])

-- Note that this instance is probably a Bad Idea.  STM allows random variables
-- to interact in spooky quantum-esque ways - One transaction can 'retry' until
-- it gets a \"random\" answer it likes, which causes it to selectively consume 
-- entropy, biasing the supply from which other random variables will draw.
-- instance (Monad m, ModifyRef (TVar PureMT) m PureMT) => RandomSource m (TVar PureMT) where
--     {-# SPECIALIZE instance RandomSource IO  (TVar PureMT) #-}
--     {-# SPECIALIZE instance RandomSource STM (TVar PureMT) #-}
--     getRandomPrimFrom = getRandomPrimFromMTRef


-- |Given a mutable reference to a 'PureMT' generator, we can implement
-- 'RandomSource' for it in any monad in which the reference can be modified.
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
-- Example usage (using some functions from "Data.Random" in the random-fu 
-- package):
-- 
-- > main = do
-- >     src <- newIORef (pureMT 1234)          -- OR: newPureMT >>= newIORef
-- >     x <- runRVar (uniform 0 100) src :: IO Double
-- >     print x
getRandomPrimFromMTRef :: ModifyRef sr m PureMT => sr -> Prim a -> m a
getRandomPrimFromMTRef ref
    = atomicModifyReference' ref 
    . runState 
    . getRandomPrim

atomicModifyReference' :: ModifyRef sr m a => sr -> (a -> (b, a)) -> m b
atomicModifyReference' ref getR =
    atomicModifyReference ref (swap' . getR)
        where swap' (!a,!b) = (b,a)
