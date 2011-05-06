{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Random.RVar
    ( RVar, runRVar
    , RVarT, runRVarT, runRVarTWith
    ) where

import Data.Random.Lift
import Data.Random.Internal.Source
import Data.RVar hiding (runRVar, runRVarT)
import qualified Data.RVar as R

-- |\"Run\" an 'RVar' - samples the random variable from the provided
-- source of entropy.  Typically 'sample', 'sampleFrom' or 'sampleState' will
-- be more convenient to use.
runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar x src = R.runRVar x (getRandomPrimFrom src)

-- |Like 'runRVarTWith', but using an implicit lifting (provided by the 
-- 'Lift' class)
runRVarT :: (Lift n m, RandomSource m s) => RVarT n a -> s -> m a
runRVarT x src = R.runRVarT x lift (getRandomPrimFrom src)

-- | \"Runs\" an 'RVarT', sampling the random variable it defines.
-- 
-- The first argument lifts the base monad into the sampling monad.  This 
-- operation must obey the \"monad transformer\" laws:
--
-- > lift . return = return
-- > lift (x >>= f) = (lift x) >>= (lift . f)
--
-- One example of a useful non-standard lifting would be one that takes
-- @State s@ to another monad with a different state representation (such as
-- @IO@ with the state mapped to an @IORef@):
--
-- > embedState :: (Monad m) => m s -> (s -> m ()) -> State s a -> m a
-- > embedState get put = \m -> do
-- >     s <- get
-- >     (res,s) <- return (runState m s)
-- >     put s
-- >     return res
--
-- The lifting is very important - without it, every 'RVar' would have
-- to either be given access to the full capability of the monad in which it
-- will eventually be sampled (which, incidentally, would also have to be 
-- monomorphic so you couldn't sample one 'RVar' in more than one monad)
-- or functions manipulating 'RVar's would have to use higher-ranked 
-- types to enforce the same kind of isolation and polymorphism.
-- 
-- The second argument specifies a 'RandomSource' to use.
runRVarTWith :: RandomSource m s => (forall t. n t -> m t) -> RVarT n a -> s -> m a
runRVarTWith liftN x src = R.runRVarT x liftN (getRandomPrimFrom src)
