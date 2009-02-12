{-
 -      ``Data/Random/Distribution''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, KindSignatures
  #-}

module Data.Random.Distribution where

class Distribution (d :: * -> *) t