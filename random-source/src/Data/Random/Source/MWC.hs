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

import Data.Random.Internal.Words
import Data.Random.Source
import System.Random.MWC
import Control.Monad.ST

instance RandomSource (ST s) (Gen s) where
    getRandomPrimFrom src = getPrimWhere supported (getPrim src)
        where
            {-# INLINE supported #-}
            supported :: Prim a -> Bool
            supported PrimWord8  = True
            supported PrimWord16 = True
            supported PrimWord32 = True
            supported PrimWord64 = True
            supported PrimDouble = True
            supported _ = False
    
            {-# INLINE getPrim #-}
            getPrim :: Gen s -> Prim a -> ST s a
            getPrim gen PrimWord8    = uniform gen
            getPrim gen PrimWord16   = uniform gen
            getPrim gen PrimWord32   = uniform gen
            getPrim gen PrimWord64   = uniform gen
            getPrim gen PrimDouble   = fmap wordToDouble (uniform gen)
            getPrim   _ p            = error ("getRandomPrimFrom/Gen s: unsupported prim requested: " ++ show p)

instance RandomSource IO (Gen RealWorld) where
    getRandomPrimFrom src = getPrimWhere supported (getPrim src)
        where
            {-# INLINE supported #-}
            supported :: Prim a -> Bool
            supported PrimWord8  = True
            supported PrimWord16 = True
            supported PrimWord32 = True
            supported PrimWord64 = True
            supported PrimDouble = True
            supported _ = False
    
            {-# INLINE getPrim #-}
            getPrim :: Gen RealWorld -> Prim a -> IO a
            getPrim gen PrimWord8    = uniform gen
            getPrim gen PrimWord16   = uniform gen
            getPrim gen PrimWord32   = uniform gen
            getPrim gen PrimWord64   = uniform gen
            getPrim gen PrimDouble   = fmap wordToDouble (uniform gen)
            getPrim   _ p            = error ("getRandomPrimFrom/Gen RealWorld: unsupported prim requested: " ++ show p)

