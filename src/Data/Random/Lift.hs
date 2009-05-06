{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}

module Data.Random.Lift where

import Control.Monad.Identity
import qualified Control.Monad.Trans as T

-- | A class for \"liftable\" data structures. Conceptually
-- an extension of 'T.MonadTrans' to allow deep lifting,
-- but lifting need not be done between monads only. Eg lifting
-- between 'Applicative's is allowed.
--
-- For instances where 'm' and 'n' have 'return'/'pure' defined,
-- these instances must satisfy
-- @lift (return x) == return x@.
class Lift m n where
    lift :: m a -> n a

instance (Monad m, T.MonadTrans t) => Lift m (t m) where
    lift = T.lift

instance Lift m m where
    lift = id

-- | This instance is incoherent with the other two. However,
-- by the law @lift (return x) == return x@, the results
-- must always be the same.
instance Monad m => Lift Identity m where
    lift = return . runIdentity