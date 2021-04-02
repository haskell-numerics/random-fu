{-# LANGUAGE GADTs, RankNTypes, DeriveDataTypeable #-}
-- |This is an internal interface to support the 'RVar' abstraction.  It
-- reifies the operations provided by both MonadRandom and RandomSource in a
-- uniform and efficient way, as functions of type @Prim a -> m a@.
module Data.Random.Source.Internal.Prim (Prim(..)) where

import Data.Word
import Data.Typeable

-- |A 'Prompt' GADT describing a request for a primitive random variate.
-- Random variable definitions will request their entropy via these prompts,
-- and entropy sources will satisfy those requests.  The 'Integer' 0 <= U < 256^n'Integer' 0 <= U < 256^n'Integer' 0 <= U < 256^nfunctions in
-- "Data.Random.Source.Internal.TH" extend incomplete entropy-source definitions
-- to complete ones, essentially defining a very flexible 
-- implementation-defaulting system.
-- 
-- Some possible future additions:
--    PrimFloat :: Prim Float
--    PrimInt :: Prim Int
--    PrimPair :: Prim a -> Prim b -> Prim (a :*: b)
--    PrimNormal :: Prim Double
--    PrimChoice :: [(Double :*: a)] -> Prim a
--    PrimBytes  :: !Int -> Prim ByteString
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
    -- A uniformly distributed 'Integer' 0 <= U < 256^n
    PrimNByteInteger    :: !Int -> Prim Integer
    deriving (Typeable)

instance Show (Prim a) where
    showsPrec _p PrimWord8               = showString "PrimWord8"
    showsPrec _p PrimWord16              = showString "PrimWord16"
    showsPrec _p PrimWord32              = showString "PrimWord32"
    showsPrec _p PrimWord64              = showString "PrimWord64"
    showsPrec _p PrimDouble              = showString "PrimDouble"
    showsPrec  p (PrimNByteInteger n)    = showParen (p > 10) (showString "PrimNByteInteger " . showsPrec 11 n)
