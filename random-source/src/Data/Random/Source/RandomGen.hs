
{-# LANGUAGE
    CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs,
    BangPatterns, RankNTypes,
    ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |This module provides functions useful for implementing new 'MonadRandom'
-- and 'RandomSource' instances for state-abstractions containing over 'RandomGen'
-- instance (the type class for pseudorandom generators provided by the System.Random
-- module in the \"random\" package), as well as instances for some common
-- cases.
module Data.Random.Source.RandomGen
    ( getRandomPrimFromRandomGenRef
    , getRandomPrimFromRandomGenState
    ) where

import Data.Random.Internal.Source
import System.Random
import Control.Monad.State
import Control.Monad.RWS
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.RWS.Strict as S
import Data.StateRef
import Data.Word


instance (Monad m1, RandomGen g, ModifyRef (Ref m2 g) m1 g) => RandomSource m1 (Ref m2 g) where
    getRandomPrimFrom = getRandomPrimFromRandomGenRef

instance (Monad m, RandomGen g, ModifyRef (IORef g) m g) => RandomSource m (IORef g) where
    {-# SPECIALIZE instance RandomSource IO (IORef StdGen) #-}
    getRandomPrimFrom = getRandomPrimFromRandomGenRef

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
getRandomPrimFromRandomGenRef :: (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> Prim a -> m a
getRandomPrimFromRandomGenRef ref
    = atomicModifyReference' ref
    . runState
    . getRandomPrimFromRandomGenState

atomicModifyReference' :: ModifyRef sr m a => sr -> (a -> (b, a)) -> m b
atomicModifyReference' ref getR =
    atomicModifyReference ref (swap' . getR)
        where swap' (!a,!b) = (b,a)


-- |Similarly, @getRandomWordFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
{-# INLINABLE getRandomPrimFromRandomGenState #-}
-- making the function inlinable enables specialization in other modules
getRandomPrimFromRandomGenState :: forall g m a. (RandomGen g, MonadState g m) => Prim a -> m a
getRandomPrimFromRandomGenState = genPrim
    where
        {-# INLINE genPrim #-}
        genPrim :: forall t. Prim t -> m t
        genPrim PrimWord8            = getThing (randomR (0, 0xff))                (fromIntegral :: Int -> Word8)
        genPrim PrimWord16           = getThing (randomR (0, 0xffff))              (fromIntegral :: Int -> Word16)
        genPrim PrimWord32           = getThing (randomR (0, 0xffffffff))          (fromInteger)
        genPrim PrimWord64           = getThing (randomR (0, 0xffffffffffffffff))  (fromInteger)
        genPrim PrimDouble           = getThing (randomR (0, 0x000fffffffffffff))  (flip encodeFloat (-52))
          {- not using the Random Double instance for 2 reasons.  1st, it only generates 32 bits of entropy, when
             a [0,1) Double has room for 52.  Second, it appears there's a bug where it can actually generate a
             negative number in the case where randomIvalInteger returns minBound::Int32. -}
--        genPrim PrimDouble = getThing (randomR (0, 1.0))  (id)
        genPrim (PrimNByteInteger n) = getThing (randomR (0, iterate (*256) 1 !! n)) id

        {-# INLINE getThing #-}
        getThing :: forall b t. (g -> (b, g)) -> (b -> t) -> m t
        getThing thing f = do
            !oldGen <- get
            case thing oldGen of
                (!i,!newGen) -> do
                    put newGen
                    return (f $! i)

#ifndef MTL2
instance RandomGen g => MonadRandom (State g) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance RandomGen g => MonadRandom (S.State g) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance (RandomGen g, Monoid w) => MonadRandom (RWS r w g) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance (RandomGen g, Monoid w) => MonadRandom (S.RWS r w g) where
    getRandomPrim = getRandomPrimFromRandomGenState
#endif

instance (RandomGen g, Monad m) => MonadRandom (StateT g m) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance (RandomGen g, Monad m) => MonadRandom (S.StateT g m) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance (RandomGen g, Monad m, Monoid w) => MonadRandom (RWST r w g m) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance (RandomGen g, Monad m, Monoid w) => MonadRandom (S.RWST r w g m) where
    getRandomPrim = getRandomPrimFromRandomGenState
