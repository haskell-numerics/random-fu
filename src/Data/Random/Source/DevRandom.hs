{-
 -      ``Data/Random/Source/DevRandom''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, GADTs
  #-}

module Data.Random.Source.DevRandom 
    ( DevRandom(..)
    ) where

import Data.Random.Source
import Data.Random.Internal.Primitives

import System.IO (openBinaryFile, hGetBuf, IOMode(..))
import Foreign

-- |On systems that have it, \/dev\/random is a handy-dandy ready-to-use source
-- of nonsense.  Keep in mind that on some systems, Linux included, \/dev\/random
-- collects \"real\" entropy, and if you don't have a good source of it, such as
-- special hardware for the purpose or a *lot* of network traffic, it's pretty easy
-- to suck the entropy pool dry with entropy-intensive applications.  For many
-- purposes other than cryptography, \/dev\/urandom is preferable because when it
-- runs out of real entropy it'll still churn out pseudorandom data.
data DevRandom = DevRandom | DevURandom

{-# NOINLINE devRandom  #-}
devRandom  = unsafePerformIO (openBinaryFile "/dev/random"  ReadMode)
{-# NOINLINE devURandom #-}
devURandom = unsafePerformIO (openBinaryFile "/dev/urandom" ReadMode)

dev DevRandom  = devRandom
dev DevURandom = devURandom

instance RandomSource IO DevRandom where
    supportedPrimsFrom _ PrimWord8          = True
    supportedPrimsFrom _ PrimWord32         = True
    supportedPrimsFrom _ PrimWord64         = True
    supportedPrimsFrom _ _ = False
    
    getSupportedRandomPrimFrom src PrimWord8  = allocaBytes 1 $ \buf -> do
        1 <- hGetBuf (dev src) buf  1
        peek buf
    getSupportedRandomPrimFrom src PrimWord32  = allocaBytes 1 $ \buf -> do
        4 <- hGetBuf (dev src) buf  4
        peek (castPtr buf)
    getSupportedRandomPrimFrom src PrimWord64  = allocaBytes 8 $ \buf -> do
        8 <- hGetBuf (dev src) buf  8
        peek (castPtr buf)
