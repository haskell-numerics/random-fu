{-
 -      ``Data/Random/Source''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, GADTs
  #-}

module Data.Random.Source
    ( MonadRandom(..)
    , RandomSource(..)
    ) where

import Data.Word
import Control.Monad
import Control.Monad.Prompt

import Data.Random.Internal.Words
import Data.Random.Internal.Primitives

-- |A typeclass for monads with a chosen source of entropy.  For example,
-- 'RVar' is such a monad - the source from which it is (eventually) sampled
-- is the only source from which a random variable is permitted to draw, so
-- when directly requesting entropy for a random variable these functions
-- are used.
-- 
-- The minimal definition is 'supportedPrims' and either 'getRandomPrim' or
-- the corresponding functions for the primitives indicated as supported.
class Monad m => MonadRandom m where
    -- |Predicate indicating whether a given primitive is supported by the
    -- instance.  The first parameter is a phantom used to select the instance.
    supportedPrims :: m () -> Prim t -> Bool
    
    -- |Generate a random value corresponding to the specified primitive
    getRandomPrim :: Prim t -> m t
    getRandomPrim prim = val
        where
            val = runPromptM dispatchPrim (decomposePrimWhere (supportedPrims mPhantom) prim)
            
            mPhantom = error "supportedPrims tried to evaluate a phantom parameter" `asTypeOf` (val >> return ())
            
            dispatchPrim :: MonadRandom m => Prim t -> m t
            dispatchPrim PrimWord8     = getRandomByte
            dispatchPrim PrimWord64    = getRandomWord
            dispatchPrim PrimDouble    = getRandomDouble


    -- |Get a random uniformly-distributed byte.
    getRandomByte :: m Word8
    getRandomByte = getRandomPrim PrimWord8
    
    -- |Get a random 'Word64' uniformly-distributed over the full range of the type.
    getRandomWord :: m Word64
    getRandomWord = getRandomPrim PrimWord64
    
    -- |Get a random 'Double' uniformly-distributed over the interval [0,1)
    getRandomDouble :: m Double
    getRandomDouble = getRandomPrim PrimDouble

-- |A source of entropy which can be used in the given monad.
--
-- The minimal definition is 'supportedPrimsFrom' and either 'getRandomPrimFrom' or
-- the corresponding functions for the primitives indicated as supported.
class Monad m => RandomSource m s where
    -- |Predicate indicating whether a given primitive is supported by the
    -- instance.  The first parameter is a phantom used to select the instance.
    supportedPrimsFrom :: m s -> Prim t -> Bool
    
    -- |Generate a random value corresponding to the specified primitive
    getRandomPrimFrom :: s -> Prim t -> m t
    getRandomPrimFrom src prim = val
        where
            val = runPromptM (dispatchPrim src) (decomposePrimWhere (supportedPrimsFrom mPhantom) prim)
            
            mPhantom = error "supportedPrims tried to evaluate a phantom parameter" `asTypeOf` (val >> return src)
            
            {-# INLINE dispatchPrim #-}
            dispatchPrim :: RandomSource m s => s -> Prim t -> m t
            dispatchPrim src PrimWord8     = getRandomByteFrom src
            dispatchPrim src PrimWord64    = getRandomWordFrom src
            dispatchPrim src PrimDouble    = getRandomDoubleFrom src


    -- |Get a random uniformly-distributed byte.
    getRandomByteFrom :: s -> m Word8
    getRandomByteFrom src = getRandomPrimFrom src PrimWord8
    
    -- |Get a random 'Word64' uniformly-distributed over the full range of the type.
    getRandomWordFrom :: s -> m Word64
    getRandomWordFrom src = getRandomPrimFrom src PrimWord64
    
    -- |Get a random 'Double' uniformly-distributed over the interval [0,1)
    getRandomDoubleFrom :: s -> m Double
    getRandomDoubleFrom src = getRandomPrimFrom src PrimDouble

instance Monad m => RandomSource m (m Word8) where
    supportedPrimsFrom _ PrimWord8 = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom = id

instance Monad m => RandomSource m (m Word64) where
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ _ = False
    
    getRandomWordFrom = id

instance Monad m => RandomSource m (m Double) where
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomDoubleFrom = id

