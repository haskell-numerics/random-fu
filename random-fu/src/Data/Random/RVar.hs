{-# LANGUAGE RankNTypes #-}
module Data.Random.RVar
    ( module Data.RVar
    , module Data.Random.RVar
    ) where

import Data.Random.Lift
import Data.Random.Source
import Data.RVar hiding (runRVar, runRVarT, runRVarTWith)
import qualified Data.RVar as R

runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar x src = R.runRVar x (getRandomPrimFrom src)

runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT x src = R.runRVarT x (getRandomPrimFrom src)

runRVarTWith :: RandomSource m s => (forall t. n t -> m t) -> RVarT n a -> s -> m a
runRVarTWith liftN x src = R.runRVarTWith x liftN (getRandomPrimFrom src)
