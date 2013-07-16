{-# LANGUAGE
    TemplateHaskell, GADTs, RankNTypes,
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Internal.Source
    ( Prim(..)
    , MonadRandom(..)
    , RandomSource(..)
    , GetPrim(..)
    ) where

import Data.Random.Source.Internal.Prim
import Data.Word

-- |A typeclass for monads with a chosen source of entropy.  For example,
-- 'RVar' is such a monad - the source from which it is (eventually) sampled
-- is the only source from which a random variable is permitted to draw, so
-- when directly requesting entropy for a random variable these functions
-- are used.
--
-- Minimum implementation is either the internal 'getRandomPrim' or all
-- other functions.  Additionally, this class's interface is subject to 
-- extension at any time, so it is very, very strongly recommended that
-- the 'monadRandom' Template Haskell function be used to implement this 
-- function rather than directly implementing it.  That function takes care
-- of choosing default implementations for any missing functions; as long as
-- at least one function is implemented, it will derive sensible 
-- implementations of all others.
-- 
-- To use 'monadRandom', just wrap your instance declaration as follows (and
-- enable the TemplateHaskell and GADTs language extensions):
--
-- > $(monadRandom [d|
-- >         instance MonadRandom FooM where
-- >             getRandomDouble = return pi
-- >             getRandomWord16 = return 4
-- >             {- etc... -}
-- >     |])
class Monad m => MonadRandom m where
    -- |Generate a random value corresponding to the specified primitive.
    -- 
    -- This is an internal interface; use at your own risk.  It may change or
    -- disappear at any time.
    getRandomPrim :: Prim t -> m t
    getRandomPrim PrimWord8             = getRandomWord8
    getRandomPrim PrimWord16            = getRandomWord16
    getRandomPrim PrimWord32            = getRandomWord32
    getRandomPrim PrimWord64            = getRandomWord64
    getRandomPrim PrimDouble            = getRandomDouble
    getRandomPrim (PrimNByteInteger n)  = getRandomNByteInteger n
    
    -- |Generate a uniformly distributed random 'Word8'
    getRandomWord8 :: m Word8
    getRandomWord8 = getRandomPrim PrimWord8
    
    -- |Generate a uniformly distributed random 'Word16'
    getRandomWord16 :: m Word16
    getRandomWord16 = getRandomPrim PrimWord16
    
    -- |Generate a uniformly distributed random 'Word32'
    getRandomWord32 :: m Word32
    getRandomWord32 = getRandomPrim PrimWord32
    
    -- |Generate a uniformly distributed random 'Word64'
    getRandomWord64 :: m Word64
    getRandomWord64 = getRandomPrim PrimWord64
    
    -- |Generate a uniformly distributed random 'Double' in the range 0 <= U < 1
    getRandomDouble :: m Double
    getRandomDouble = getRandomPrim PrimDouble
    
    -- |Generate a uniformly distributed random 'Integer' in the range 0 <= U < 256^n
    getRandomNByteInteger :: MonadRandom m => Int -> m Integer
    getRandomNByteInteger n = getRandomPrim (PrimNByteInteger n)


-- |A source of entropy which can be used in the given monad.
-- 
-- See also 'MonadRandom'.
-- 
-- Minimum implementation is either the internal 'getRandomPrimFrom' or all
-- other functions.  Additionally, this class's interface is subject to 
-- extension at any time, so it is very, very strongly recommended that
-- the 'randomSource' Template Haskell function be used to implement this 
-- function rather than directly implementing it.  That function takes care
-- of choosing default implementations for any missing functions; as long as
-- at least one function is implemented, it will derive sensible 
-- implementations of all others.
-- 
-- To use 'randomSource', just wrap your instance declaration as follows (and
-- enable the TemplateHaskell, MultiParamTypeClasses and GADTs language
-- extensions, as well as any others required by your instances, such as
-- FlexibleInstances):
--
-- > $(randomSource [d|
-- >         instance RandomSource FooM Bar where
-- >             {- at least one RandomSource function... -}
-- >     |])
class Monad m => RandomSource m s where
    -- |Generate a random value corresponding to the specified primitive.
    -- 
    -- This is an internal interface; use at your own risk.  It may change or
    -- disappear at any time.
    getRandomPrimFrom :: s -> Prim t -> m t
    getRandomPrimFrom src PrimWord8             = getRandomWord8From  src
    getRandomPrimFrom src PrimWord16            = getRandomWord16From src
    getRandomPrimFrom src PrimWord32            = getRandomWord32From src
    getRandomPrimFrom src PrimWord64            = getRandomWord64From src
    getRandomPrimFrom src PrimDouble            = getRandomDoubleFrom src
    getRandomPrimFrom src (PrimNByteInteger n)  = getRandomNByteIntegerFrom src n
    
    
    -- |Generate a uniformly distributed random 'Word8'
    getRandomWord8From :: s -> m Word8
    getRandomWord8From src = getRandomPrimFrom src PrimWord8
    
    -- |Generate a uniformly distributed random 'Word16'
    getRandomWord16From :: s -> m Word16
    getRandomWord16From src = getRandomPrimFrom src PrimWord16
    
    -- |Generate a uniformly distributed random 'Word32'
    getRandomWord32From :: s -> m Word32
    getRandomWord32From src = getRandomPrimFrom src PrimWord32
    
    -- |Generate a uniformly distributed random 'Word64'
    getRandomWord64From :: s -> m Word64
    getRandomWord64From src = getRandomPrimFrom src PrimWord64
    
    -- |Generate a uniformly distributed random 'Double' in the range 0 <= U < 1
    getRandomDoubleFrom :: s -> m Double
    getRandomDoubleFrom src = getRandomPrimFrom src PrimDouble
    
    -- |Generate a uniformly distributed random 'Integer' in the range 0 <= U < 256^n
    getRandomNByteIntegerFrom :: s -> Int -> m Integer
    getRandomNByteIntegerFrom src n = getRandomPrimFrom src (PrimNByteInteger n)

-- |This type provides a way to define a 'RandomSource' for a monad without actually
-- having to declare an instance.
newtype GetPrim m = GetPrim (forall t. Prim t -> m t)
instance Monad m => RandomSource m (GetPrim m) where
    getRandomPrimFrom (GetPrim f) = f