{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |This is an internal interface to support the 'RVar' abstraction.  It
-- reifies the operations provided by `System.Random.Stateful.StatefulGen` in a
-- uniform and efficient way, as functions of type @Prim a -> m a@.
module Data.RVar.Prim (Prim(..)) where

import Data.Typeable
import Data.Word
import Data.ByteString.Short

-- |A 'Prompt' GADT describing a request for a primitive random variate.  Random variable
-- definitions will request their entropy via these prompts, and entropy sources will
-- satisfy those requests. This data type is needed for creating
-- `System.Random.Stateful.StatefulGen` instance for `Data.RVar.RVarT`
--
data Prim a where
    -- | An unsigned byte, uniformly distributed from 0 to 0xff
    PrimWord8           :: Prim Word8
    -- | An unsigned 16-bit word, uniformly distributed from 0 to 0xffff
    PrimWord16          :: Prim Word16
    -- | An unsigned 32-bit word, uniformly distributed from 0 to 0xffffffff
    PrimWord32          :: Prim Word32
    -- | An unsigned 64-bit word, uniformly distributed from 0 to 0xffffffffffffffff
    PrimWord64          :: Prim Word64
    -- | A uniformly distributed `ShortByteString` of length @n@ bytes
    PrimShortByteString :: !Int -> Prim ShortByteString
    deriving (Typeable)

instance Show (Prim a) where
    showsPrec _p PrimWord8         = showString "PrimWord8"
    showsPrec _p PrimWord16        = showString "PrimWord16"
    showsPrec _p PrimWord32        = showString "PrimWord32"
    showsPrec _p PrimWord64        = showString "PrimWord64"
    showsPrec  p (PrimShortByteString n) =
      showParen (p > 10) (showString "PrimShortByteString " . showsPrec 11 n)
