{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        GADTs
  #-}
module Data.Random.Source.MWC where

import Data.Random.Internal.Primitives
import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST

instance RandomSource (ST s) (Gen s) where
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = uniform
    getRandomWordFrom   = uniform
    getRandomDoubleFrom = fmap wordToDouble . uniform

instance RandomSource IO (Gen RealWorld) where
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    getRandomByteFrom   = stToIO . getRandomByteFrom
    getRandomWordFrom   = stToIO . getRandomWordFrom
    getRandomDoubleFrom = stToIO . getRandomDoubleFrom
