{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |This is an internal interface to support the 'RVar' abstraction.  It
-- reifies the operations provided by `System.Random.Stateful.StatefulGen` in a
-- uniform and efficient way, as functions of type @Prim a -> m a@.
module Data.RVar.Prim (Prim(..)) where

import Data.Word

-- |A type describing a request for a primitive random variate. This
-- data type is needed for creating
-- `System.Random.Stateful.StatefulGen` instance for `Data.RVar.RVarT`

data Prim a = Prim (Word32 -> a)

instance Functor Prim where
  fmap f (Prim k) = Prim (f . k)
