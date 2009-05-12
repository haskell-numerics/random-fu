{-
 -      ``Data/Random/Source/DevRandom''
 -}
{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Data.Random.Source.DevRandom 
    ( DevRandom(..)
    ) where

import Data.Random.Source

import GHC.IOBase (unsafePerformIO)
import Data.ByteString (hGet, unpack)
import System.IO (openBinaryFile, IOMode(..))

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

instance RandomSource IO DevRandom where
    getRandomBytesFrom DevRandom  n = fmap unpack (hGet devRandom  n)
    getRandomBytesFrom DevURandom n = fmap unpack (hGet devURandom n)
