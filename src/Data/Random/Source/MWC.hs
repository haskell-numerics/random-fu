{-# LANGUAGE
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

import Data.Random.Internal.Primitives
import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST

instance RandomSource (ST s) (Gen s) where
    {-# INLINE supportedPrimsFrom #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord16 = True
    supportedPrimsFrom _ PrimWord32 = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    {-# INLINE getSupportedRandomPrimFrom #-}
    getSupportedRandomPrimFrom gen PrimWord8    = uniform gen
    getSupportedRandomPrimFrom gen PrimWord16   = uniform gen
    getSupportedRandomPrimFrom gen PrimWord32   = uniform gen
    getSupportedRandomPrimFrom gen PrimWord64   = uniform gen
    getSupportedRandomPrimFrom gen PrimDouble   = fmap wordToDouble (uniform gen)
    getSupportedRandomPrimFrom _ p = error ("getSupportedRandomPrimFrom/Gen s: unsupported prim requested: " ++ show p)

instance RandomSource IO (Gen RealWorld) where
    {-# INLINE supportedPrimsFrom #-}
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord16 = True
    supportedPrimsFrom _ PrimWord32 = True
    supportedPrimsFrom _ PrimWord64 = True
    supportedPrimsFrom _ PrimDouble = True
    supportedPrimsFrom _ _ = False
    
    {-# INLINE getSupportedRandomPrimFrom #-}
    getSupportedRandomPrimFrom gen prim = stToIO (getSupportedRandomPrimFrom gen prim)
