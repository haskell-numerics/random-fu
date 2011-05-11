{-# LANGUAGE
    CPP,
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
module Data.Random.Source.StdGen where

import Data.Random.Internal.Words
import Data.Random.Internal.Source
import System.Random
import Control.Monad.State
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.State.Strict as S
import Data.StateRef
import Data.Word


instance (Monad m1, ModifyRef (Ref m2 StdGen) m1 StdGen) => RandomSource m1 (Ref m2 StdGen) where
    getRandomPrimFrom = getRandomPrimFromRandomGenRef

instance (Monad m, ModifyRef (IORef   StdGen) m StdGen) => RandomSource m (IORef   StdGen) where
    {-# SPECIALIZE instance RandomSource IO (IORef StdGen) #-}
    getRandomPrimFrom = getRandomPrimFromRandomGenRef

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
    getRandomPrimFrom = getRandomPrimFromRandomGenRef

getRandomPrimFromStdGenIO :: Prim a -> IO a
getRandomPrimFromStdGenIO = genPrim
    where 
        {-# INLINE genPrim #-}
        genPrim :: Prim a -> IO a
        genPrim PrimWord8            = fmap fromIntegral                  (randomRIO (0, 0xff) :: IO Int)
        genPrim PrimWord16           = fmap fromIntegral                  (randomRIO (0, 0xffff) :: IO Int)
        genPrim PrimWord32           = fmap fromInteger                   (randomRIO (0, 0xffffffff))
        genPrim PrimWord64           = fmap fromInteger                   (randomRIO (0, 0xffffffffffffffff))
        genPrim PrimDouble           = fmap (wordToDouble . fromInteger)  (randomRIO (0, 0xffffffffffffffff))
        genPrim (PrimNByteInteger n) = randomRIO (0, iterate (*256) 1 !! n)

-- |Given a mutable reference to a 'RandomGen' generator, we can make a
-- 'RandomSource' usable in any monad in which the reference can be modified.
-- 
-- See "Data.Random.Source.PureMT".'getRandomPrimFromMTRef' for more detailed
-- usage hints - this function serves exactly the same purpose except for a
-- 'StdGen' generator instead of a 'PureMT' generator.
getRandomPrimFromRandomGenRef :: forall sr m g a. (Monad m, ModifyRef sr m g, RandomGen g) =>
                                  sr -> Prim a -> m a
getRandomPrimFromRandomGenRef ref = genPrim
    where 
        {-# INLINE genPrim #-}
        genPrim :: forall t. Prim t -> m t
        genPrim PrimWord8            = getThing (randomR (0, 0xff))                (fromIntegral :: Int -> Word8)
        genPrim PrimWord16           = getThing (randomR (0, 0xffff))              (fromIntegral :: Int -> Word16)
        genPrim PrimWord32           = getThing (randomR (0, 0xffffffff))          (fromInteger)
        genPrim PrimWord64           = getThing (randomR (0, 0xffffffffffffffff))  (fromInteger)
        genPrim PrimDouble           = getThing (randomR (0, 0x000fffffffffffff))  (flip encodeFloat (-52))
        genPrim (PrimNByteInteger n) = getThing (randomR (0, iterate (*256) 1 !! n)) (id :: Integer -> Integer)
        
        {-# INLINE getThing #-}
        getThing :: forall b t. (g -> (b, g)) -> (b -> t) -> m t
        getThing thing f = atomicModifyReference ref $ \(!oldMT) -> case thing oldMT of (!w, !newMT) -> (newMT, f w)


-- |Similarly, @getRandomWordFromRandomGenState x@ can be used in any \"state\"
-- monad in the mtl sense whose state is a 'RandomGen' generator.
-- Additionally, the standard mtl state monads have 'MonadRandom' instances
-- which do precisely that, allowing an easy conversion of 'RVar's and
-- other 'Distribution' instances to \"pure\" random variables.
-- 
-- Again, see "Data.Random.Source.PureMT".'getRandomPrimFromMTState' for more
-- detailed usage hints - this function serves exactly the same purpose except 
-- for a 'StdGen' generator instead of a 'PureMT' generator.
{-# SPECIALIZE getRandomPrimFromRandomGenState :: Prim a -> State StdGen a #-}
{-# SPECIALIZE getRandomPrimFromRandomGenState :: Monad m => Prim a -> StateT StdGen m a #-}
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
instance MonadRandom (State StdGen) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance MonadRandom (S.State StdGen) where
    getRandomPrim = getRandomPrimFromRandomGenState
#endif

instance Monad m => MonadRandom (StateT StdGen m) where
    getRandomPrim = getRandomPrimFromRandomGenState

instance Monad m => MonadRandom (S.StateT StdGen m) where
    getRandomPrim = getRandomPrimFromRandomGenState

