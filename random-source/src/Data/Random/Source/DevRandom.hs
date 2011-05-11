{-# LANGUAGE
    TemplateHaskell, MultiParamTypeClasses, GADTs
  #-}

module Data.Random.Source.DevRandom 
    ( DevRandom(..)
    ) where

import Data.Random.Source

import System.IO (openBinaryFile, hGetBuf, Handle, IOMode(..))
import Foreign

-- |On systems that have it, \/dev\/random is a handy-dandy ready-to-use source
-- of nonsense.  Keep in mind that on some systems, Linux included, \/dev\/random
-- collects \"real\" entropy, and if you don't have a good source of it, such as
-- special hardware for the purpose or a *lot* of network traffic, it's pretty easy
-- to suck the entropy pool dry with entropy-intensive applications.  For many
-- purposes other than cryptography, \/dev\/urandom is preferable because when it
-- runs out of real entropy it'll still churn out pseudorandom data.
data DevRandom = DevRandom | DevURandom
    deriving (Eq, Show)

{-# NOINLINE devRandom  #-}
devRandom :: Handle
devRandom  = unsafePerformIO (openBinaryFile "/dev/random"  ReadMode)
{-# NOINLINE devURandom #-}
devURandom :: Handle
devURandom = unsafePerformIO (openBinaryFile "/dev/urandom" ReadMode)

dev :: DevRandom -> Handle
dev DevRandom  = devRandom
dev DevURandom = devURandom

$(randomSource
    [d| instance RandomSource IO DevRandom where
            getRandomWord8From src = allocaBytes 1 $ \buf -> do
                1 <- hGetBuf (dev src) buf  1
                peek buf
            getRandomWord16From src = allocaBytes 2 $ \buf -> do
                2 <- hGetBuf (dev src) buf  2
                peek (castPtr buf)
            getRandomWord32From src = allocaBytes 4 $ \buf -> do
                4 <- hGetBuf (dev src) buf  4
                peek (castPtr buf)
            getRandomWord64From src = allocaBytes 8 $ \buf -> do
                8 <- hGetBuf (dev src) buf  8
                peek (castPtr buf)
     |])
