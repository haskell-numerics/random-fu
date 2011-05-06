{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, GADTs
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , RandomSource(..)
    , Prim(..)
    , getPrimWhere
    , monadRandom, randomSource
    ) where

import Data.Word

import Data.Random.Source.Prim
import Data.Random.Source.TH

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
    getRandomPrim = error "getRandomPrim not implemented"

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
    getRandomPrimFrom = error "getRandomPrimFrom not implemented"

$(randomSource
    [d| instance Monad m => RandomSource m (m Word8) |]
    [d| getWord8 = id |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Word16) |]
    [d| getWord16 = id |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Word32) |]
    [d| getWord32 = id |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Word64) |]
    [d| getWord64 = id |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Double) |]
    [d| getDouble = id |])
