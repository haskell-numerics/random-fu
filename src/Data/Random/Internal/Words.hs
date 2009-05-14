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

wordsToBytes :: [Word64] -> [Word8]
wordsToBytes = concatMap wordToBytes

wordToBytes :: Word64 -> [Word8]
wordToBytes x = unsafePerformIO . allocaBytes 8 $ \p -> do
    poke (castPtr p) x
    mapM (peekElemOff p) [0..7]

bytesToWords :: [Word8] -> [Word64]
bytesToWords = map bytesToWord . chunk 8
    where
        chunk n [] = []
        chunk n xs = case splitAt n xs of
            (ys, zs) -> ys : chunk n zs

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

bytesToWord :: [Word8] -> Word64
bytesToWord bs = unsafePerformIO . allocaBytes 8 $ \p -> do
    zipWithM (pokeElemOff p) [0..7] (bs ++ repeat 0)
    peek (castPtr p)

concatBytes :: (Bits a, Num a) => [Word8] -> a
concatBytes = concatBits fromIntegral

concatWords :: (Bits a, Num a) => [Word64] -> a
concatWords = concatBits fromIntegral

concatBits :: (Bits a, Bits b, Num b) => (a -> b) -> [a] -> b
concatBits f [] = 0
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)
