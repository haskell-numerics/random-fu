{-
 -      ``Data/Random/Source/Std''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Data.Random.Source.Std where

import Data.Random.Source
import Data.Tagged

-- |A token representing the \"standard\" entropy source in a 'MonadRandom'
-- monad.  Its sole purpose is to make the following true (when the types check):
--
-- > sampleFrom StdRandom === sample
data StdRandom = StdRandom

instance MonadRandom m => RandomSource m StdRandom where
    {-SPECIALIZE instance MonadRandom m => RandomSource m StdRandom -}
    supportedPrimsFrom w = supportedPrims (mkWit w)
        where
            mkWit :: Tagged a b -> a
            mkWit = error "supportedPrims tried to evaluate its phantom parameter"
    getSupportedRandomPrimFrom   StdRandom = getSupportedRandomPrim
    
    getRandomPrimFrom StdRandom = getRandomPrim
