{-
 -      ``Data/Random/RVar''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}

module Data.Random.RVar where

import Data.Random.Source

data RVar a
instance MonadRandom RVar