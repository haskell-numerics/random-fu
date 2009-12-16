{-
 -      ``Data/Random/Internal/Words''
 -}

-- |A few little functions I found myself writing inline over and over again.
module Data.Random.Internal.Words where

import Foreign
import GHC.IOBase

import Data.Bits
import Data.Word

{-# INLINE buildWord #-}
-- |Build a word out of 8 bytes.  No promises are made regarding the order
-- in which the bytes are stuffed.  Note that this means that a 'RandomSource'
-- or 'MonadRandom' making use of the default definition of 'getRandomWord', etc.,
-- may return different random values on different platforms when started 
-- with the same seed, depending on the platform's endianness.
buildWord :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
buildWord b0 b1 b2 b3 b4 b5 b6 b7
    = unsafePerformIO . allocaBytes 8 $ \p -> do
        pokeByteOff p 0 b0
        pokeByteOff p 1 b1
        pokeByteOff p 2 b2
        pokeByteOff p 3 b3
        pokeByteOff p 4 b4
        pokeByteOff p 5 b5
        pokeByteOff p 6 b6
        pokeByteOff p 7 b7
        peek (castPtr p)

{-# INLINE wordToFloat #-}
-- |Pack the low 23 bits from a 'Word64' into a 'Float' in the range [0,1).
-- Used to convert a 'stdUniform' 'Word64' to a 'stdUniform' 'Double'.
wordToFloat :: Word64 -> Float
wordToFloat x = (encodeFloat $! toInteger (x .&. 0x007fffff {- 2^23-1 -} )) $ (-23)

{-# INLINE wordToFloatWithExcess #-}
-- |Same as wordToFloat, but also return the unused bits (as the 41
-- least significant bits of a 'Word64')
wordToFloatWithExcess :: Word64 -> (Float, Word64)
wordToFloatWithExcess x = (wordToFloat x, x `shiftR` 23)

{-# INLINE wordToDouble #-}
-- |Pack the low 52 bits from a 'Word64' into a 'Double' in the range [0,1).
-- Used to convert a 'stdUniform' 'Word64' to a 'stdUniform' 'Double'.
wordToDouble :: Word64 -> Double
wordToDouble x = (encodeFloat $! toInteger (x .&. 0x000fffffffffffff {- 2^52-1 -})) $ (-52)

{-# INLINE wordToDoubleWithExcess #-}
-- |Same as wordToDouble, but also return the unused bits (as the 12
-- least significant bits of a 'Word64')
wordToDoubleWithExcess :: Word64 -> (Double, Word64)
wordToDoubleWithExcess x = (wordToDouble x, x `shiftR` 52)

