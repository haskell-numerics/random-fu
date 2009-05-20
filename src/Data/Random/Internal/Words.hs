{-
 -      ``Data/Random/Internal/Words''
 -}

-- |A few little functions I found myself writing inline over and over again.
--
-- Note that these need to be checked to ensure proper behavior on big-endian 
-- systems.  They are probably not right at the moment.
module Data.Random.Internal.Words where

import Foreign
import GHC.IOBase

import Data.Bits
import Data.Word
import Control.Monad

{-# INLINE buildWord #-}
buildWord :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
buildWord b0 b1 b2 b3 b4 b5 b6 b7
    = unsafePerformIO . allocaBytes 8 $ \p -> do
        pokeElemOff p 0 b0
        pokeElemOff p 1 b1
        pokeElemOff p 2 b2
        pokeElemOff p 3 b3
        pokeElemOff p 4 b4
        pokeElemOff p 5 b5
        pokeElemOff p 6 b6
        pokeElemOff p 7 b7
        peek (castPtr p)

{-# INLINE wordToFloat #-}
wordToFloat :: Word64 -> Float
wordToFloat x = (encodeFloat $! toInteger (x `shiftR` ( 41 {- 64-23 -}))) $ (-23)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = (encodeFloat $! toInteger (x `shiftR` ( 12 {- 64-52 -}))) $ (-52)

