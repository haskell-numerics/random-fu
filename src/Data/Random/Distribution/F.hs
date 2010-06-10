{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}
module Data.Random.Distribution.F where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Control.Monad
import Data.List

data F t = F Int Int

instance (Fractional t, Distribution Beta t) => Distribution F t where
    rvar (F p q) = do
        let p' = fromIntegral p; q' = fromIntegral q
        y <- beta (0.5*p') (0.5*q')
        return (q'/p' * y/(1-y))
    