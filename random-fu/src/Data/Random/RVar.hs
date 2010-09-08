{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Random.RVar
    ( module Data.RVar
    , module Data.Random.RVar
    ) where

import Data.Random.Lift
import Data.Random.Source
import Data.RVar hiding (runRVar, runRVarT)
import qualified Data.RVar as R
import qualified Data.Functor.Identity as T
import qualified Control.Monad.Identity as MTL
import Control.Monad.Prompt (prompt)

runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar x src = R.runRVar x (getRandomPrimFrom src)

runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT x src = R.runRVarT x lift (getRandomPrimFrom src)

runRVarTWith :: RandomSource m s => (forall t. n t -> m t) -> RVarT n a -> s -> m a
runRVarTWith liftN x src = R.runRVarT x liftN (getRandomPrimFrom src)

instance Lift (RVarT T.Identity) (RVarT m) where
    lift x = R.runRVar x prompt

instance Lift (RVarT MTL.Identity) (RVarT m) where
    lift x = R.runRVarT x lift prompt 

