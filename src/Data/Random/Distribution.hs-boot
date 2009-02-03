{-
 -      ``Data/Random/Distribution''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, KindSignatures
  #-}

module Data.Random.Distribution where

class Distribution (d :: * -> *) t