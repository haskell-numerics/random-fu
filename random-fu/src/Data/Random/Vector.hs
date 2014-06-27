module Data.Random.Vector(randomElement) where

import           Data.Random.RVar
import           Data.Random.Distribution.Uniform
import qualified Data.Vector  as V
import           Control.Applicative

-- | Take a random element of a vector.
randomElement :: V.Vector a -> RVar a
randomElement words = (words V.!) <$> uniform 0 (V.length words - 1)
