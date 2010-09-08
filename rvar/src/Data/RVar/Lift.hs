{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}

module Data.RVar.Lift where

import qualified Data.Functor.Identity as T
import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.Identity as MTL

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

-- | This instance is incoherent with the other two. However,
-- by the law @lift (return x) == return x@, the results
-- must always be the same.
instance Monad m => Lift T.Identity m where
    lift = return . T.runIdentity

-- | This instance is incoherent with the other two. However,
-- by the law @lift (return x) == return x@, the results
-- must always be the same.
instance Monad m => Lift MTL.Identity m where
    lift = return . MTL.runIdentity

