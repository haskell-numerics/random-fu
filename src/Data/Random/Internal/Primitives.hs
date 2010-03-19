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
module Data.Random.Internal.Primitives (Prim(..), decomposePrimWhere) where

import Data.Random.Internal.Words
import Data.Word
import Data.Bits
import Data.Typeable

import Control.Monad.Prompt

data Prim a where
    -- An unsigned byte, uniformly distributed from 0 to 255
    PrimWord8           :: Prim Word8
    -- An unsigned 32-bit word, uniformly distributed from 0 to 2^32-1
    PrimWord32          :: Prim Word32
    -- An unsigned 64-bit word, uniformly distributed from 0 to 2^64-1
    PrimWord64          :: Prim Word64
    -- A double-precision float U, uniformly distributed 0 <= U < 1
    PrimDouble          :: Prim Double
    -- A uniformly distributed 'Integer' 0 <= U < 2^(8*n)
    PrimNByteInteger    :: Int -> Prim Integer
  
-- Some suggested future additions  
--    PrimFloat :: Prim Float
--    PrimWord16 :: Prim Word16
--    PrimInt :: Prim Int
--    PrimPair :: Prim a -> Prim b -> Prim (a :*: b)
--    PrimVec :: Vector v a => Int -> Prim a -> Prim v a
    deriving (Typeable)

instance Show (Prim a) where
    showsPrec p PrimWord8               = showString "PrimWord8"
    showsPrec p PrimWord32              = showString "PrimWord32"
    showsPrec p PrimWord64              = showString "PrimWord64"
    showsPrec p PrimDouble              = showString "PrimDouble"
    showsPrec p (PrimNByteInteger n)    = showParen (p > 10) (showString "PrimNByteInteger " . showsPrec 11 n)

-- This is essentially a suite of interrelated default implementations,
-- each definition making use of only 'supported' primitives.  It _really_
-- ought to be inlined to the point where the 'supported' predicate
-- is able to be inlined into it and eliminated.  
-- 
-- When inlined sufficiently, it should in theory be optimized down to the
-- static set of "best" definitions for each required primitive in terms of 
-- only supported primitives.
-- 
-- Hopefully, when not inlined, it does not impose too much overhead.
{-# INLINE decomposePrimWhere #-}
decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim a -> Prompt Prim a
decomposePrimWhere supported requested = decomp requested
    where
        {-# INLINE decomp #-}
        -- First, all supported prims should just be evaluated directly.
        decomp :: Prim a -> Prompt Prim a
        decomp prim
            | supported prim = prompt prim
        -- beyond this point, all definitions must be in terms of
        -- 'prompt's referring to other supported primitives or 
        -- 'decomp's referring to other primitives in a well-founded way
        
        decomp PrimWord8
            | supported PrimWord32 = do
                w <- prompt PrimWord32
                return (fromIntegral w)
            | supported PrimWord64 = do
                w <- prompt PrimWord64
                return (fromIntegral w)
            | supported PrimDouble = do
                d <- prompt PrimDouble
                return (truncate (d * 256))
        
        decomp PrimWord32
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
        
        decomp PrimWord64
            | supported PrimWord32 = do
                w0 <- prompt PrimWord32
                w1 <- prompt PrimWord32
                
                return (buildWord64'' w0 w1)
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
                d0 <- prompt PrimDouble
                d1 <- prompt PrimDouble
                
                let w0 = truncate (d0 * 4294967296)
                    w1 = truncate (d1 * 4294967296)
                
                return (w0 .|. (w1 `shiftL` 32))
        
        decomp PrimDouble = do
            word <- decomp PrimWord64
            return (wordToDouble word)
        
        decomp (PrimNByteInteger 1) = do
            x <- decomp PrimWord8
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
        decomp (PrimNByteInteger (n+1))  = do
            x <- decomp PrimWord8
            y <- decomp (PrimNByteInteger n)
            return $! (toInteger x `shiftL` (n `shiftL` 3)) .|. y
                
        
        decomp _ = error ("decomposePrimWhere: no supported primitive to satisfy " ++ show requested)
