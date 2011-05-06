{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, GADTs
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Random.Source
    ( MonadRandom
    , getRandomWord8
    , getRandomWord16
    , getRandomWord32
    , getRandomWord64
    , getRandomDouble
    , getRandomNByteInteger
    
    , RandomSource
    , getRandomWord8From
    , getRandomWord16From
    , getRandomWord32From
    , getRandomWord64From
    , getRandomDoubleFrom
    , getRandomNByteIntegerFrom
    
    , monadRandom, randomSource
    ) where

import Data.Word

import Data.Random.Internal.Source
import Data.Random.Internal.Prim
import Data.Random.Source.TH

getRandomWord8 :: MonadRandom m => m Word8
getRandomWord8 = getRandomPrim PrimWord8

getRandomWord16 :: MonadRandom m => m Word16
getRandomWord16 = getRandomPrim PrimWord16

getRandomWord32 :: MonadRandom m => m Word32
getRandomWord32 = getRandomPrim PrimWord32

getRandomWord64 :: MonadRandom m => m Word64
getRandomWord64 = getRandomPrim PrimWord64

getRandomDouble :: MonadRandom m => m Double
getRandomDouble = getRandomPrim PrimDouble

getRandomNByteInteger :: MonadRandom m => Int -> m Integer
getRandomNByteInteger n = getRandomPrim (PrimNByteInteger n)

getRandomWord8From :: RandomSource m s => s -> m Word8
getRandomWord8From src = getRandomPrimFrom src PrimWord8

getRandomWord16From :: RandomSource m s => s -> m Word16
getRandomWord16From src = getRandomPrimFrom src PrimWord16

getRandomWord32From :: RandomSource m s => s -> m Word32
getRandomWord32From src = getRandomPrimFrom src PrimWord32

getRandomWord64From :: RandomSource m s => s -> m Word64
getRandomWord64From src = getRandomPrimFrom src PrimWord64

getRandomDoubleFrom :: RandomSource m s => s -> m Double
getRandomDoubleFrom src = getRandomPrimFrom src PrimDouble

getRandomNByteIntegerFrom :: RandomSource m s => s -> Int -> m Integer
getRandomNByteIntegerFrom src n = getRandomPrimFrom src (PrimNByteInteger n)

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
