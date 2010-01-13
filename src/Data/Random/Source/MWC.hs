{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}
module Data.Random.Source.MWC where

import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST

instance RandomSource (ST s) (Gen s) where
    getRandomByteFrom   = uniform
    getRandomWordFrom   = uniform
    getRandomDoubleFrom = fmap wordToDouble . uniform

instance RandomSource IO (Gen RealWorld) where
    getRandomByteFrom   = stToIO . getRandomByteFrom
    getRandomWordFrom   = stToIO . getRandomWordFrom
    getRandomDoubleFrom = stToIO . getRandomDoubleFrom
