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

import Data.Word
import Control.Monad
import Control.Monad.Prompt
import Data.Tagged

import Data.Random.Internal.Words
import Data.Random.Internal.Primitives

-- |A typeclass for monads with a chosen source of entropy.  For example,
-- 'RVar' is such a monad - the source from which it is (eventually) sampled
-- is the only source from which a random variable is permitted to draw, so
-- when directly requesting entropy for a random variable these functions
-- are used.
-- 
-- The minimal definition is 'supportedPrims' and 'getSupportedRandomPrim'
-- with cases for those primitives where 'supportedPrims' returns 'True'.
--
-- It is recommended (despite the warnings it generates) that, even when
-- all primitives are supported, a final wildcard case of 'supportedPrims' is
-- specified, as:
-- 
-- > supportedPrims _ _ = False
--
-- The overlapping pattern warnings can be suppressed (without suppressing 
-- other, genuine, overlapping-pattern warnings) by the GHC flag
-- @-fno-warn-simple-patterns@.  This is not actually the documented behavior
-- of that flag as far as I can find in 3 google-minutes, but it works with
-- GHC 6.12.1 anyway, and that's good enough for me.
--
-- Note that it is very important that at least 'supportedPrims' (and preferably
-- 'getSupportedRandomPrim' as well) gets inlined into the default implementation
-- of 'getRandomPrim'.  If your 'supportedPrims' is more than about 2 or 3
-- cases, add an INLINE pragma so that it can be optimized out of 'getRandomPrim'.
class Monad m => MonadRandom m where
    -- |Predicate indicating whether a given primitive is supported by the
    -- instance.  The first parameter is a phantom used to select the instance.
    supportedPrims :: m () -> Prim t -> Bool
    
    -- |Generate a random value corresponding to the specified primitive.  Will
    -- not be called unless supportedPrims returns true for that primitive.
    getSupportedRandomPrim :: Prim t -> m t

    -- This could just be a function, but placing it in a dictionary gives
    -- GHC a place to optimize it separately for each instance, which is 
    -- kinda the whole point of the 'Prim' machinery:
    -- 
    -- |Generate a random value corresponding to the specified primitive.  The
    -- default implementation makes use of 'supportedPrims' and 'getSupportedRandomPrim'
    -- to construct any required Prim out of the supported ones.
    {-# NOINLINE getRandomPrim #-}
    getRandomPrim :: Prim t -> m t
    getRandomPrim prim = val
        where
            val = runPromptM getSupportedRandomPrim (decomposePrimWhere (supportedPrims mPhantom) prim)
            mPhantom = error "supportedPrims tried to evaluate a phantom parameter" `asTypeOf` (val >> return ())

-- |A source of entropy which can be used in the given monad.
--
-- The minimal definition is 'supportedPrimsFrom' and 'getSupportedRandomPrimFrom'
-- with cases for those primitives where 'supportedPrimsFrom' returns 'True'.
-- 
-- Note that it is very important that at least 'supportedPrimsFrom' (and preferably
-- 'getSupportedRandomPrimFrom' as well) gets inlined into the default implementation
-- of 'getRandomPrimFrom'.  If your 'supportedPrimsFrom' is more than about 2 or 3
-- cases, add an INLINE pragma so that it can be optimized out of 'getRandomPrimFrom'.
-- 
-- See also 'MonadRandom'.
class Monad m => RandomSource m s where
    -- |Predicate indicating whether a given primitive is supported by the
    -- instance.  The second parameter is a phantoms used only to select the instance.
    supportedPrimsFrom :: Tagged (m ()) s -> Prim t -> Bool
    
    -- |Generate a random value corresponding to the specified primitive
    getSupportedRandomPrimFrom :: s -> Prim t -> m t
    
    
    -- This could just be a function, but placing it in a dictionary gives
    -- GHC a place to optimize it separately for each instance, which is 
    -- kinda the whole point of the 'Prim' machinery:
    -- 
    -- |Generate a random value corresponding to the specified primitive.  The
    -- default implementation makes use of 'supportedPrimsFrom' and
    -- 'getSupportedRandomPrimFrom' to construct any required Prim out of 
    -- the supported ones.
    {-# NOINLINE getRandomPrimFrom #-}
    getRandomPrimFrom :: s -> Prim t -> m t
    getRandomPrimFrom src prim = val
        where
            val = runPromptM (getSupportedRandomPrimFrom src) (decomposePrimWhere supported prim)
            supported :: Prim t -> Bool
            supported = supportedPrimsFrom (tagIt (val >> return ()) src)
            
            tagIt :: a -> b -> Tagged a b
            tagIt _ it = Tagged it

instance Monad m => RandomSource m (m Word8) where
    supportedPrimsFrom _ PrimWord8 = True
    supportedPrimsFrom _ _ = False
    
    getSupportedRandomPrimFrom f PrimWord8 = f

instance Monad m => RandomSource m (m Word64) where
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ _ = False
    
    getSupportedRandomPrimFrom f PrimWord64 = f

instance Monad m => RandomSource m (m Double) where
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getSupportedRandomPrimFrom f PrimDouble = f
