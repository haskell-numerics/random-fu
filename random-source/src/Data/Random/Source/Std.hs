{-
 -      ``Data/Random/Source/Std''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source.Std where

import Data.Random.Source.Internal.Source

-- |A token representing the \"standard\" entropy source in a 'MonadRandom'
-- monad.  Its sole purpose is to make the following true (when the types check):
--
-- > runRVar x StdRandom === sampleRVar
data StdRandom = StdRandom

instance MonadRandom m => RandomSource m StdRandom where
    {-SPECIALIZE instance MonadRandom m => RandomSource m StdRandom -}
    getRandomPrimFrom StdRandom = getRandomPrim
