{-
 -      ``Data/Random/Source''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, GADTs
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , RandomSource(..)
    , Prim(..)
    ) where

import Data.RVar
import Data.Word

import Data.Random.Internal.Primitives

-- |A typeclass for monads with a chosen source of entropy.  For example,
-- 'RVar' is such a monad - the source from which it is (eventually) sampled
-- is the only source from which a random variable is permitted to draw, so
-- when directly requesting entropy for a random variable these functions
-- are used.
-- 
-- Occasionally one might want a 'RandomSource' specifying the 'MonadRandom'
-- instance (for example, when using 'runRVar').  For those cases, 
-- "Data.Random.Source.Std".'StdRandom' provides a 'RandomSource' that
-- maps to the 'MonadRandom' instance.
-- 
-- For example, @State StdGen@ has a 'MonadRandom' instance, so to run an
-- 'RVar' (called @x@ in this example) in this monad one could write
-- @runRVar x StdRandom@ (or more concisely with the 'sample' function: @sample x@).
-- 
class Monad m => MonadRandom m where
    -- |Generate a random value corresponding to the specified primitive.
    -- The 'Prim' type has many variants, and is also somewhat unstable.
    -- 'getPrimWhere' is a useful function for abstracting over the type,
    -- semi-automatically extending a partial implementation to the full
    -- 'Prim' type.
    getRandomPrim :: Prim t -> m t

instance MonadRandom (RVarT n) where
    supportedPrims _ _ = True
    getSupportedRandomPrim = prompt
    getRandomPrim          = prompt

-- |A source of entropy which can be used in the given monad.
-- 
-- See also 'MonadRandom'.
class Monad m => RandomSource m s where
    -- |Generate a random value corresponding to the specified primitive.
    -- The 'Prim' type has many variants, and is also somewhat unstable.
    -- 'getPrimWhere' is a useful function for abstracting over the type,
    -- semi-automatically extending a partial implementation to the full
    -- 'Prim' type.
    getRandomPrimFrom :: s -> Prim t -> m t

instance Monad m => RandomSource m (m Word8) where
    getRandomPrimFrom f = getPrimWhere supported (getPrim f)
        where
            supported :: Prim a -> Bool
            supported PrimWord8 = True
            supported _ = False
            
            getPrim :: m Word8 -> Prim a -> m a
            getPrim f PrimWord8 = f

instance Monad m => RandomSource m (m Word16) where
    getRandomPrimFrom f = getPrimWhere supported (getPrim f)
        where
            supported :: Prim a -> Bool
            supported PrimWord16 = True
            supported _ = False
            
            getPrim :: m Word16 -> Prim a -> m a
            getPrim f PrimWord16 = f

instance Monad m => RandomSource m (m Word32) where
    getRandomPrimFrom f = getPrimWhere supported (getPrim f)
        where
            supported :: Prim a -> Bool
            supported PrimWord32 = True
            supported _ = False
            
            getPrim :: m Word32 -> Prim a -> m a
            getPrim f PrimWord32 = f

instance Monad m => RandomSource m (m Word64) where
    getRandomPrimFrom f = getPrimWhere supported (getPrim f)
        where
            supported :: Prim a -> Bool
            supported PrimWord64 = True
            supported _ = False
            
            getPrim :: m Word64 -> Prim a -> m a
            getPrim f PrimWord64 = f

instance Monad m => RandomSource m (m Double) where
    getRandomPrimFrom f = getPrimWhere supported (getPrim f)
        where
            supported :: Prim a -> Bool
            supported PrimDouble = True
            supported _ = False
            
            getPrim :: m Double -> Prim a -> m a
            getPrim f PrimDouble = f
