{-# LANGUAGE GADTs, RankNTypes, DeriveDataTypeable #-}
-- |This is an experimental interface to support an extensible set of primitives,
-- where a RandomSource will be able to support whatever subset of them they want
-- and have well-founded defaults generated automatically for any unsupported
-- primitives.
module Data.Random.Internal.Primitives where

import Data.Random.Internal.Words
import Data.Word
import Data.Bits
import Data.Typeable

import Control.Monad.Prompt

data Prim a where
    -- |An unsigned byte, uniformly distributed from 0 to 255
    PrimWord8       :: Prim Word8
    -- |An unsigned 64-bit word, uniformly distributed from 0 to 2^64-1
    PrimWord64      :: Prim Word64
    -- |A double-precision float U, uniformly distributed 0 <= U < 1
    PrimDouble      :: Prim Double
    deriving (Typeable)

instance Show (Prim a) where
    showsPrec p PrimWord8  = showString "PrimWord8"
    showsPrec p PrimWord64 = showString "PrimWord64"
    showsPrec p PrimDouble = showString "PrimDouble"

{-# INLINE decomposePrimWhere #-}
decomposePrimWhere :: (forall t. Prim t -> Bool) -> Prim a -> Prompt Prim a
decomposePrimWhere supported requested = decomp requested
    where
        {-# INLINE decomp #-}
        decomp :: Prim a -> Prompt Prim a
        decomp prim
            | supported prim = prompt prim
        
        decomp PrimWord8
            | supported PrimWord64 = do
                w <- prompt PrimWord64
                return (fromIntegral w)
            | supported PrimDouble = do
                d <- prompt PrimDouble
                return (truncate (d * 256))
            
        decomp PrimWord64
            | supported PrimWord8 = do
                b0 <- prompt PrimWord8
                b1 <- prompt PrimWord8
                b2 <- prompt PrimWord8
                b3 <- prompt PrimWord8
                b4 <- prompt PrimWord8
                b5 <- prompt PrimWord8
                b6 <- prompt PrimWord8
                b7 <- prompt PrimWord8
                
                return (buildWord b0 b1 b2 b3 b4 b5 b6 b7)
            | supported PrimDouble = do
                d0 <- prompt PrimDouble
                d1 <- prompt PrimDouble
                
                let w0 = truncate (d0 * 4294967296)
                    w1 = truncate (d1 * 4294967296)
                
                return (w0 .|. (w1 `shiftL` 32))
        
        decomp PrimDouble = do
            word <- decomp PrimWord64
            return (wordToDouble word)


        decomp _ = error ("decomposePrimWhere: no supported primitive to satisfy " ++ show requested)
