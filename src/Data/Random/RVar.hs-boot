{-
 -      ``Data/Random/RVar''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances
  #-}

module Data.Random.RVar where

import Data.Random.Source
import {-# SOURCE #-} Data.Random.Distribution

data RVar a
instance MonadRandom RVar
instance Distribution RVar a