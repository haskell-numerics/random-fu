{-# LANGUAGE
        TemplateHaskell,
        MultiParamTypeClasses,
        FlexibleInstances,
        GADTs
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |This module defines the following instances:
-- 
-- > instance RandomSource (ST s) (Gen s)
-- > instance RandomSource IO (Gen RealWorld)
module Data.Random.Source.MWC
    ( Gen, RealWorld
    , create, initialize
    , save, restore
    ) where

import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Primitive

$(randomSource
    [d| instance RandomSource (ST s) (Gen s) where
            getRandomWord8From  = uniform
            getRandomWord16From = uniform
            getRandomWord32From = uniform
            getRandomWord64From = uniform
            getRandomDoubleFrom = fmap wordToDouble . uniform
     |])

$(randomSource
    [d| instance RandomSource IO (Gen RealWorld) where
            getRandomWord8From  = uniform
            getRandomWord16From = uniform
            getRandomWord32From = uniform
            getRandomWord64From = uniform
            getRandomDoubleFrom = fmap wordToDouble . uniform
     |])

$(monadRandom [d|
  instance (PrimMonad m, s ~ PrimState m) => MonadRandom (ReaderT (Gen s) m) where
    getRandomWord16 = ask >>= lift . uniform
    getRandomWord32 = ask >>= lift . uniform
    getRandomWord64 = ask >>= lift . uniform
  |])