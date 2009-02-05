{-
 -      ``Data/Random/Internal/Words''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}

module Data.Random.Internal.Words where

import Foreign
import GHC.IOBase

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


bytesToWord :: [Word8] -> Word64
bytesToWord bs = unsafePerformIO . allocaBytes 8 $ \p -> do
    zipWithM (pokeElemOff p) [0..7] (bs ++ repeat 0)
    peek (castPtr p)
