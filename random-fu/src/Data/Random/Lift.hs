{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances, CPP #-}

module Data.Random.Lift where

import Data.RVar
import qualified Data.Functor.Identity as T
import qualified Control.Monad.Trans.Class as T

#ifndef MTL2
import qualified Control.Monad.Identity as MTL
#endif

-- | A class for \"liftable\" data structures. Conceptually
-- an extension of 'T.MonadTrans' to allow deep lifting,
-- but lifting need not be done between monads only. Eg lifting
-- between 'Applicative's is allowed.
--
-- For instances where 'm' and 'n' have 'return'/'pure' defined,
-- these instances must satisfy
-- @lift (return x) == return x@.
--
-- This form of 'lift' has an extremely general type and is used primarily to
-- support 'sample'.  Its excessive generality is the main reason it's not
-- exported from "Data.Random".  'RVarT' is, however, an instance of
-- 'T.MonadTrans', which in most cases is the preferred way
-- to do the lifting.
class Lift m n where
    lift :: m a -> n a

instance (Monad m, T.MonadTrans t) => Lift m (t m) where
    lift = T.lift

instance Lift m m where
    lift = id

-- | This instance is incoherent with the others. However,
-- by the law @lift (return x) == return x@, the results
-- must always be the same.
instance Monad m => Lift T.Identity m where
    lift = return . T.runIdentity

instance Functor m => Lift (RVarT T.Identity) (RVarT m) where
    lift x = runRVar x RGen

-- | This instance is again incoherent with the others, but provides a
-- more-specific instance to resolve the overlap between the
-- @Lift m (t m)@ and @Lift Identity m@ instances.
instance T.MonadTrans t => Lift T.Identity (t T.Identity) where
    lift = T.lift

