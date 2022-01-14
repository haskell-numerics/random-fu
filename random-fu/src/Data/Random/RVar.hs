{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Random.RVar
    ( RVar, runRVar
    , RVarT, runRVarT, runRVarTWith
    , RGen(..), uniformRVarT, uniformRangeRVarT
    ) where

import Data.Random.Lift
import Data.RVar hiding (runRVarT)
import System.Random.Stateful

-- |Like 'runRVarTWith', but using an implicit lifting (provided by the
-- 'Lift' class)
runRVarT :: (Lift n m, StatefulGen g m) => RVarT n a -> g -> m a
runRVarT = runRVarTWith lift
