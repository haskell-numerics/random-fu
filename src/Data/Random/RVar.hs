{-
 -      ``Data/Random/RVar''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances
  #-}

module Data.Random.RVar
    ( RVar
    ) where

import Data.Random.Distribution
import Data.Random.Source

newtype RVar a = RVar { runDistM :: forall m s. RandomSource m s => s -> m a }

instance Monad RVar where
    return x = RVar (\_ -> return x)
    fail s   = RVar (\_ -> fail s)
    (RVar x) >>= f = RVar (\s -> do
            x <- x s
            case f x of
                RVar y -> y s
        )
        

instance Distribution RVar a where
    rvar = id
    sampleFrom src (RVar x) = x src

instance MonadRandom RVar where
    getRandomBytes n = RVar (\s -> getRandomBytesFrom s n)