{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, GADTs
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Random.Source
    ( MonadRandom
        ( getRandomWord8
        , getRandomWord16
        , getRandomWord32
        , getRandomWord64
        , getRandomDouble
        , getRandomNByteInteger
        )

    , RandomSource
        ( getRandomWord8From
        , getRandomWord16From
        , getRandomWord32From
        , getRandomWord64From
        , getRandomDoubleFrom
        , getRandomNByteIntegerFrom
        )

    , monadRandom, randomSource
    ) where

import Data.Word

import Data.Random.Internal.Source
import Data.Random.Source.Internal.TH

$(randomSource
    [d|
        instance Monad m => RandomSource m (m Word8) where
            getRandomWord8From = id
     |])

$(randomSource
    [d|
        instance Monad m => RandomSource m (m Word16) where
            getRandomWord16From = id
     |])

$(randomSource
    [d|
        instance Monad m => RandomSource m (m Word32) where
            getRandomWord32From = id
     |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Word64) where
            getRandomWord64From = id
     |])

$(randomSource
    [d| instance Monad m => RandomSource m (m Double) where
            getRandomDoubleFrom = id
     |])
