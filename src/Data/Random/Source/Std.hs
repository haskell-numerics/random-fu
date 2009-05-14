{-
 -      ``Data/Random/Source/Std''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source.Std where

import Data.Random.Source

-- |A token representing the \"standard\" entropy source in a 'MonadRandom'
-- monad.  Its sole purpose is to make the following true (when the types check):
--
-- > sampleFrom StdRandom === sample
data StdRandom = StdRandom

instance MonadRandom m => RandomSource m StdRandom where
    getRandomByteFrom StdRandom = getRandomByte
    getRandomWordFrom StdRandom = getRandomWord
