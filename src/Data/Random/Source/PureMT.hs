{-# LANGUAGE
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

import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S

-- |Given a mutable reference to a 'PureMT' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
--
-- For example, if @x :: TVar PureMT@, @getRandomWordFromMTRef x@ can be
-- used as a 'RandomSource' in 'IO', 'STM', or any monad which is an instance
-- of 'MonadIO'.  These functions can also be used to implement additional
-- 'RandomSource' instances for mutable references to 'PureMT' states.
getRandomWordFromMTRef :: ModifyRef sr m PureMT => sr -> m Word64
getRandomWordFromMTRef ref = do
    atomicModifyReference ref (swap . randomWord64)
    
    where
        swap (a,b) = (b,a)

getRandomByteFromMTRef :: (Monad m, ModifyRef sr m PureMT) => sr -> m Word8
getRandomByteFromMTRef ref = do
    x <- atomicModifyReference ref (swap . randomInt)
    return (fromIntegral x)
    
    where
        swap (a,b) = (b,a)

-- for whatever reason, my simple wordToDouble is faster than whatever
-- the mersenne random library is using, at least in the version I have.
-- if this changes, switch to the commented version.
-- Same thing below, in getRandomDoubleFromMTState.
getRandomDoubleFromMTRef :: (Monad m, ModifyRef sr m PureMT) => sr -> m Double
getRandomDoubleFromMTRef src = liftM wordToDouble (getRandomWordFromMTRef src)
-- getRandomDoubleFromMTRef ref = do
--     atomicModifyReference ref (swap . randomDouble)
--     
--     where
--         swap (a,b) = (b,a)

-- |Similarly, @getRandomWordFromMTState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'PureMT' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables (e.g., by
-- @runState . sample :: Distribution d t => d t -> PureMT -> (t, PureMT)@.
-- 'PureMT' in the type there can be replaced by 'StdGen' or anything else 
-- satisfying @MonadRandom (State s) => s@).
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


getRandomDoubleFromMTState :: MonadState PureMT m => m Double
getRandomDoubleFromMTState = liftM wordToDouble getRandomWordFromMTState
-- getRandomDoubleFromMTState = do
--     mt <- get
--     let (x, newMt) = randomDouble mt
--     put newMt
--     return x

instance MonadRandom (State PureMT) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromMTState
    getRandomWord   = getRandomWordFromMTState
    getRandomDouble = getRandomDoubleFromMTState

instance MonadRandom (S.State PureMT) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromMTState
    getRandomWord   = getRandomWordFromMTState
    getRandomDouble = getRandomDoubleFromMTState

instance (Monad m1, ModifyRef (Ref m2 PureMT) m1 PureMT) => RandomSource m1 (Ref m2 PureMT) where
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromMTRef
    getRandomWordFrom   = getRandomWordFromMTRef
    getRandomDoubleFrom = getRandomDoubleFromMTRef

instance Monad m => MonadRandom (StateT PureMT m) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromMTState
    getRandomWord   = getRandomWordFromMTState
    getRandomDouble = getRandomDoubleFromMTState

instance Monad m => MonadRandom (S.StateT PureMT m) where
    supportedPrims _ PrimWord8  = True
    supportedPrims _ PrimWord64 = True
    supportedPrims _ PrimDouble = True
    supportedPrims _ _ = False
    
    getRandomByte   = getRandomByteFromMTState
    getRandomWord   = getRandomWordFromMTState
    getRandomDouble = getRandomDoubleFromMTState

instance (Monad m, ModifyRef (IORef PureMT) m PureMT) => RandomSource m (IORef PureMT) where
    {-# SPECIALIZE instance RandomSource IO (IORef PureMT)#-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromMTRef
    getRandomWordFrom   = getRandomWordFromMTRef
    getRandomDoubleFrom = getRandomDoubleFromMTRef

instance (Monad m, ModifyRef (STRef s PureMT) m PureMT) => RandomSource m (STRef s PureMT) where
    {-# SPECIALIZE instance RandomSource (ST s) (STRef s PureMT) #-}
    {-# SPECIALIZE instance RandomSource (S.ST s) (STRef s PureMT) #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromMTRef
    getRandomWordFrom   = getRandomWordFromMTRef
    getRandomDoubleFrom = getRandomDoubleFromMTRef

instance (Monad m, ModifyRef (TVar PureMT) m PureMT) => RandomSource m (TVar PureMT) where
    {-# SPECIALIZE instance RandomSource IO  (TVar PureMT) #-}
    {-# SPECIALIZE instance RandomSource STM (TVar PureMT) #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = getRandomByteFromMTRef
    getRandomWordFrom   = getRandomWordFromMTRef
    getRandomDoubleFrom = getRandomDoubleFromMTRef

