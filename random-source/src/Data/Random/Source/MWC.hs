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
module Data.Random.Source.MWC where

import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST

$(randomSource
    [d| instance RandomSource (ST s) (Gen s) |]
    [d|
            getWord8  = uniform
            getWord16 = uniform
            getWord32 = uniform
            getWord64 = uniform
            getDouble = fmap wordToDouble . uniform
     |])

$(randomSource
    [d| instance RandomSource IO (Gen RealWorld) |]
    [d|
            getWord8  = uniform
            getWord16 = uniform
            getWord32 = uniform
            getWord64 = uniform
            getDouble = fmap wordToDouble . uniform
     |])
