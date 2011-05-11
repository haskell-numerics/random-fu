{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, GADTs
  #-}

module Data.Random.Internal.Source
    ( Prim(..)
    , MonadRandom(..)
    , RandomSource(..)
    ) where

import Data.Random.Internal.Prim
import Data.Word

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
    getRandomPrim PrimWord8             = getRandomWord8
    getRandomPrim PrimWord16            = getRandomWord16
    getRandomPrim PrimWord32            = getRandomWord32
    getRandomPrim PrimWord64            = getRandomWord64
    getRandomPrim PrimDouble            = getRandomDouble
    getRandomPrim (PrimNByteInteger n)  = getRandomNByteInteger n
    
    
    getRandomWord8 :: m Word8
    getRandomWord8 = getRandomPrim PrimWord8
    
    getRandomWord16 :: m Word16
    getRandomWord16 = getRandomPrim PrimWord16
    
    getRandomWord32 :: m Word32
    getRandomWord32 = getRandomPrim PrimWord32
    
    getRandomWord64 :: m Word64
    getRandomWord64 = getRandomPrim PrimWord64
    
    getRandomDouble :: m Double
    getRandomDouble = getRandomPrim PrimDouble
    
    getRandomNByteInteger :: MonadRandom m => Int -> m Integer
    getRandomNByteInteger n = getRandomPrim (PrimNByteInteger n)


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
    getRandomPrimFrom src PrimWord8             = getRandomWord8From  src
    getRandomPrimFrom src PrimWord16            = getRandomWord16From src
    getRandomPrimFrom src PrimWord32            = getRandomWord32From src
    getRandomPrimFrom src PrimWord64            = getRandomWord64From src
    getRandomPrimFrom src PrimDouble            = getRandomDoubleFrom src
    getRandomPrimFrom src (PrimNByteInteger n)  = getRandomNByteIntegerFrom src n
    
    
    getRandomWord8From :: s -> m Word8
    getRandomWord8From src = getRandomPrimFrom src PrimWord8
    
    getRandomWord16From :: s -> m Word16
    getRandomWord16From src = getRandomPrimFrom src PrimWord16
    
    getRandomWord32From :: s -> m Word32
    getRandomWord32From src = getRandomPrimFrom src PrimWord32
    
    getRandomWord64From :: s -> m Word64
    getRandomWord64From src = getRandomPrimFrom src PrimWord64
    
    getRandomDoubleFrom :: s -> m Double
    getRandomDoubleFrom src = getRandomPrimFrom src PrimDouble
    
    getRandomNByteIntegerFrom :: s -> Int -> m Integer
    getRandomNByteIntegerFrom src n = getRandomPrimFrom src (PrimNByteInteger n)

