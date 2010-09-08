{-# LANGUAGE GADTs, RankNTypes, DeriveDataTypeable #-}
-- |This is an experimental interface to support an extensible set of primitives,
-- where a RandomSource will be able to support whatever subset of them they want
-- and have well-founded defaults generated automatically for any unsupported
-- primitives.
--
-- The purpose, in case it's not clear, is to decouple the implementations of
-- entropy sources from any particular set of primitives, so that implementors
-- of random variates can make use of a large number of primitives, supported
-- on all entropy sources, while the burden on entropy-source implementors
-- is only to provide one or two basic primitives of their choice.
-- 
-- One challenge I foresee with this interface is optimization - different 
-- compilers or even different versions of GHC may treat this interface 
-- radically differently, making it very hard to achieve reliable performance
-- on all platforms.  It may even be that no compiler optimizes sufficiently
-- to make the flexibility this system provides worth the overhead.  I hope
-- this is not the case, but if it turns out to be a major problem, this
-- system may disappear or be modified in significant ways.
module Data.Random.Internal.Primitives (Prim(..), getPrimWhere, decomposePrimWhere) where

import Data.Random.Internal.Words
import Data.Word
import Data.Bits
import Data.Typeable

import Control.Monad.Prompt

-- |A 'Prompt' GADT describing a request for a primitive random variate.
-- Random variable definitions will request their entropy via these prompts,
-- and entropy sources will satisfy some or all of them.  The 'decomposePrimWhere'
-- function extends an entropy source's incomplete definition to a complete 
-- definition, essentially defining a very flexible implementation-defaulting
-- system.
-- 
-- Some possible future additions:
--    PrimFloat :: Prim Float
--    PrimInt :: Prim Int
--    PrimPair :: Prim a -> Prim b -> Prim (a :*: b)
--    PrimNormal :: Prim Double
--    PrimChoice :: [(Double :*: a)] -> Prim a
--
-- Unfortunately, I cannot get Haddock to accept my comments about the 
-- data constructors, but hopefully they should be reasonably self-explanatory.
data Prim a where
    -- An unsigned byte, uniformly distributed from 0 to 0xff
    PrimWord8           :: Prim Word8
    -- An unsigned 16-bit word, uniformly distributed from 0 to 0xffff
    PrimWord16          :: Prim Word16
    -- An unsigned 32-bit word, uniformly distributed from 0 to 0xffffffff
    PrimWord32          :: Prim Word32
    -- An unsigned 64-bit word, uniformly distributed from 0 to 0xffffffffffffffff
    PrimWord64          :: Prim Word64
    -- A double-precision float U, uniformly distributed 0 <= U < 1
    PrimDouble          :: Prim Double
    -- A uniformly distributed 'Integer' 0 <= U < 2^(8*n)
    PrimNByteInteger    :: !Int -> Prim Integer
    deriving (Typeable)

instance Show (Prim a) where
    showsPrec _p PrimWord8               = showString "PrimWord8"
    showsPrec _p PrimWord16              = showString "PrimWord16"
    showsPrec _p PrimWord32              = showString "PrimWord32"
    showsPrec _p PrimWord64              = showString "PrimWord64"
    showsPrec _p PrimDouble              = showString "PrimDouble"
    showsPrec  p (PrimNByteInteger n)    = showParen (p > 10) (showString "PrimNByteInteger " . showsPrec 11 n)

-- |This function wraps up the most common calling convention for 'decomposePrimWhere'.
-- Given a predicate identifying \"supported\" 'Prim's, and a (possibly partial) 
-- function that maps those 'Prim's to implementations, derives a total function
-- mapping all 'Prim's to implementations.
{-# INLINE getPrimWhere #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Word8   -> m Word8   #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Word16  -> m Word16  #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Word32  -> m Word32  #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Word64  -> m Word64  #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Double  -> m Double  #-}
{-# SPECIALIZE getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim Integer -> m Integer #-}
getPrimWhere :: Monad m => (forall t. Prim t -> Bool) -> (forall t. Prim t -> m t) -> Prim a -> m a
getPrimWhere supported getPrim prim = runPromptM getPrim (decomposePrimWhere supported prim)

-- |This is essentially a suite of interrelated default implementations,
-- each definition making use of only \"supported\" primitives.  It _really_
-- ought to be inlined to the point where the @supported@ predicate
-- is able to be inlined into it and eliminated.  
-- 
-- When inlined sufficiently, it should in theory be optimized down to the
-- static set of "best" definitions for each required primitive in terms of 
-- only supported primitives.
-- 
-- Hopefully it does not impose too much overhead when not inlined.
{-# INLINE decomposePrimWhere #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Word8   -> Prompt Prim Word8   #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Word16  -> Prompt Prim Word16  #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Word32  -> Prompt Prim Word32  #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Word64  -> Prompt Prim Word64  #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Double  -> Prompt Prim Double  #-}
{-# SPECIALIZE decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim Integer -> Prompt Prim Integer #-}
decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim a -> Prompt Prim a
decomposePrimWhere supported requested = decomp requested
    where
        {-# INLINE decomp #-}

        {-# SPECIALIZE decomp :: Prim Word8   -> Prompt Prim Word8   #-}
        {-# SPECIALIZE decomp :: Prim Word16  -> Prompt Prim Word16  #-}
        {-# SPECIALIZE decomp :: Prim Word32  -> Prompt Prim Word32  #-}
        {-# SPECIALIZE decomp :: Prim Word64  -> Prompt Prim Word64  #-}
        {-# SPECIALIZE decomp :: Prim Double  -> Prompt Prim Double  #-}
        {-# SPECIALIZE decomp :: Prim Integer -> Prompt Prim Integer #-}
        -- First, all supported prims should just be evaluated directly.
        decomp :: Prim a -> Prompt Prim a
        decomp prim
            | supported prim = prompt prim
        -- beyond this point, all definitions must be in terms of
        -- 'prompt's referring to other supported primitives or 
        -- 'decomp's referring to other primitives in a well-founded way
        
        decomp PrimWord8
            | supported PrimWord16 = do
                w <- prompt PrimWord16
                return (fromIntegral w)
            | supported PrimWord32 = do
                w <- prompt PrimWord32
                return (fromIntegral w)
            | supported PrimWord64 = do
                w <- prompt PrimWord64
                return (fromIntegral w)
            | supported PrimDouble = do
                d <- prompt PrimDouble
                return (truncate (d * 256))
            | supported (PrimNByteInteger 1) = do
                i <- prompt (PrimNByteInteger 1)
                return (fromInteger i)
        
        decomp PrimWord16
            | supported PrimWord8 = do
                b0 <- prompt PrimWord8
                b1 <- prompt PrimWord8
                return (buildWord16 b0 b1)
            | supported PrimWord32 = do
                w <- prompt PrimWord32
                return (fromIntegral w)
            | supported PrimWord64 = do
                w <- prompt PrimWord64
                return (fromIntegral w)
            | supported PrimDouble = do
                d <- prompt PrimDouble
                return (truncate (d * 65536))
            | supported (PrimNByteInteger 2) = do
                i <- prompt (PrimNByteInteger 2)
                return (fromInteger i)
        
        decomp PrimWord32
            | supported PrimWord16 = do
                w0 <- prompt PrimWord16
                w1 <- prompt PrimWord16
                
                return (buildWord32' w0 w1)
            | supported PrimWord8 = do
                b0 <- prompt PrimWord8
                b1 <- prompt PrimWord8
                b2 <- prompt PrimWord8
                b3 <- prompt PrimWord8
                
                return (buildWord32 b0 b1 b2 b3)
            | supported PrimWord64 = do
                w <- prompt PrimWord64
                return (fromIntegral w)
            | supported PrimDouble = do
                d <- prompt PrimDouble
                return (truncate (d * 4294967296))
            | supported (PrimNByteInteger 4) = do
                i <- prompt (PrimNByteInteger 4)
                return (fromInteger i)
        
        decomp PrimWord64
            | supported PrimWord32 = do
                w0 <- prompt PrimWord32
                w1 <- prompt PrimWord32
                
                return (buildWord64'' w0 w1)
            | supported PrimWord16 = do
                w0 <- prompt PrimWord16
                w1 <- prompt PrimWord16
                w2 <- prompt PrimWord16
                w3 <- prompt PrimWord16
                
                return (buildWord64' w0 w1 w2 w3)
            | supported PrimWord8 = do
                b0 <- prompt PrimWord8
                b1 <- prompt PrimWord8
                b2 <- prompt PrimWord8
                b3 <- prompt PrimWord8
                b4 <- prompt PrimWord8
                b5 <- prompt PrimWord8
                b6 <- prompt PrimWord8
                b7 <- prompt PrimWord8
                
                return (buildWord64 b0 b1 b2 b3 b4 b5 b6 b7)
            | supported PrimDouble = do
                -- Need 2 doubles, because a uniform [0,1) double only has
                -- about 52 bits of reliable entropy
                d0 <- prompt PrimDouble
                d1 <- prompt PrimDouble
                
                let w0 = truncate (d0 * 4294967296)
                    w1 = truncate (d1 * 4294967296)
                
                return (w0 .|. (w1 `shiftL` 32))
            | supported (PrimNByteInteger 8) = do
                i <- prompt (PrimNByteInteger 8)
                return (fromInteger i)
        
        decomp PrimDouble = do
            word <- decomp PrimWord64
            return (wordToDouble word)
        
        decomp (PrimNByteInteger 1) = do
            x <- decomp PrimWord8
            return $! toInteger x
        decomp (PrimNByteInteger 2) = do
            x <- decomp PrimWord16
            return $! toInteger x
        decomp (PrimNByteInteger 4) = do
            x <- decomp PrimWord32
            return $! toInteger x
        decomp (PrimNByteInteger 8) = do
            x <- decomp PrimWord64
            return $! toInteger x
        decomp (PrimNByteInteger (n+8))  = do
            x <- decomp PrimWord64
            y <- decomp (PrimNByteInteger n)
            return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
        decomp (PrimNByteInteger (n+4))  = do
            x <- decomp PrimWord32
            y <- decomp (PrimNByteInteger n)
            return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
        decomp (PrimNByteInteger (n+2))  = do
            x <- decomp PrimWord16
            y <- decomp (PrimNByteInteger n)
            return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
-- REDUNDANT CASE
--        decomp (PrimNByteInteger (n+1))  = do
--            x <- decomp PrimWord8
--            y <- decomp (PrimNByteInteger n)
--            return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
        decomp (PrimNByteInteger _) = return 0
        
        decomp _ = error ("decomposePrimWhere: no supported primitive to satisfy " ++ show requested)
