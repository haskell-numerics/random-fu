{-
 -      ``Data/Random/Source/DevRandom''
 -}
{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Data.Random.Source.DevRandom where

import Data.Random.Source

import GHC.IOBase (unsafePerformIO)
import Data.ByteString (hGet, unpack)
import System.IO (openBinaryFile, IOMode(..))

data DevRandom = DevRandom
{-# NOINLINE devRandom #-}
devRandom = unsafePerformIO (openBinaryFile "/dev/random" ReadMode)

instance RandomSource IO DevRandom where
    getRandomBytesFrom DevRandom n = fmap unpack (hGet devRandom n)

