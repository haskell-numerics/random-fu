{-
 -      ``Data/Random/RVar''
 -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |Random variables.  An 'RVar' is a sampleable random variable.  Because
-- probability distributions form a monad, they are quite easy to work with
-- in the standard Haskell monadic styles.  For examples, see the source for
-- any of the 'Distribution' instances - they all are defined in terms of
-- 'RVar's.
{-# LANGUAGE FlexibleContexts #-}

module Data.RVar
    ( RVar
    , runRVar
    , sampleReaderRVar
    , sampleStateRVar
    , pureRVar

    , RVarT
    , runRVarT, sampleReaderRVarT, sampleStateRVarT
    , runRVarTWith
    , sampleReaderRVarTWith
    , sampleStateRVarTWith

    , RGen(..)
    , uniformRVarT
    , uniformRangeRVarT

    , Prim(..)
    ) where


import qualified Control.Monad.IO.Class as T
import Control.Monad.Reader as MTL
import Control.Monad.State as MTL
import qualified Control.Monad.Trans.Class as T
import qualified Data.Functor.Identity as T
import Data.RVar.Prim
import System.Random.Stateful

import Control.Monad.Free.Church
import Data.Functor.Sum

import Data.Word

-- |An opaque type modeling a \"random variable\" - a value
-- which depends on the outcome of some random event.  'RVar's
-- can be conveniently defined by an imperative-looking style:
--
-- > normalPair =  do
-- >     u <- stdUniform
-- >     t <- stdUniform
-- >     let r = sqrt (-2 * log u)
-- >         theta = (2 * pi) * t
-- >
-- >         x = r * cos theta
-- >         y = r * sin theta
-- >     return (x,y)
--
-- OR by a more applicative style:
--
-- > logNormal = exp <$> stdNormal
--
-- Once defined (in any style), there are several ways to sample 'RVar's:
--
-- * Using an immutable pseudo-random number generator that has an instance for `RandomGen` with
--   `StateT` monad:
--
-- >>> import qualified Data.Random as Fu (uniform)
-- >>> import System.Random (mkStdGen)
-- >>> import Control.Monad.State (runState)
-- >>> runState (sampleStateRVar (Fu.uniform 1 (100 :: Integer))) (mkStdGen 2021)
-- (79,StdGen {unStdGen = SMGen 4687568268719557181 4805600293067301895})
--
-- * Using a mutable pseud-random number generator that has an instance for `StatefulGen` with
--   `ReaderT` monad.
--
-- >>> import qualified Data.Random as Fu (uniform)
-- >>> import System.Random.MWC (create)
-- >>> import Control.Monad.Reader (runReaderT)
-- >>> import qualified Data.Vector.Storable as VS
-- >>> initialize (VS.singleton 2021) >>= runReaderT (sampleReaderRVar (uniform 1 (100 :: Integer)))
-- 8
--
type RVar = RVarT T.Identity

-- | Sample random variable using `RandomGen` generator as source of entropy
pureRVar :: RandomGen g => RVar a -> g -> (a, g)
pureRVar rvar g = runStateGen g (runRVar rvar)

-- |\"Run\" an 'RVar' - samples the random variable from the provided
-- source of entropy.
runRVar :: StatefulGen g m => RVar a -> g -> m a
runRVar = runRVarTWith (return . T.runIdentity)

-- |@sampleRVar x@ is equivalent to @runRVar x 'StdRandom'@.
sampleReaderRVar :: (StatefulGen g m, MonadReader g m) => RVar a -> m a
sampleReaderRVar = sampleReaderRVarTWith (return . T.runIdentity)

sampleStateRVar :: (RandomGen g, MonadState g m) => RVar a -> m a
sampleStateRVar = sampleStateRVarTWith (return . T.runIdentity)

-- |A random variable with access to operations in an underlying monad.  Useful
-- examples include any form of state for implementing random processes with hysteresis,
-- or writer monads for implementing tracing of complicated algorithms.
--
-- For example, a simple random walk can be implemented as an 'RVarT' 'IO' value:
--
-- > rwalkIO :: IO (RVarT IO Double)
-- > rwalkIO d = do
-- >     lastVal <- newIORef 0
-- >
-- >     let x = do
-- >             prev    <- lift (readIORef lastVal)
-- >             change  <- rvarT StdNormal
-- >
-- >             let new = prev + change
-- >             lift (writeIORef lastVal new)
-- >             return new
-- >
-- >     return x
--
-- To run the random walk it must first be initialized, after which it can be sampled as usual:
--
-- > do
-- >     rw <- rwalkIO
-- >     x <- sampleRVarT rw
-- >     y <- sampleRVarT rw
-- >     ...
--
-- The same random-walk process as above can be implemented using MTL types
-- as follows (using @import Control.Monad.Trans as MTL@):
--
-- > rwalkState :: RVarT (State Double) Double
-- > rwalkState = do
-- >     prev <- MTL.lift get
-- >     change  <- rvarT StdNormal
-- >
-- >     let new = prev + change
-- >     MTL.lift (put new)
-- >     return new
--
-- Invocation is straightforward (although a bit noisy) if you're used to MTL:
--
-- > rwalk :: Int -> Double -> StdGen -> ([Double], StdGen)
-- > rwalk count start gen =
-- >     flip evalState start .
-- >         flip runStateT gen .
-- >             sampleRVarTWith MTL.lift $
-- >                 replicateM count rwalkState
newtype RVarT m a = RVarT { unRVarT :: F (Sum Prim m) a }

runRVarT :: StatefulGen g m => RVarT m a -> g -> m a
runRVarT = runRVarTWith id


sampleStateRVarT :: (RandomGen g, MonadState g m) => RVarT m a -> m a
sampleStateRVarT rvar = runRVarT rvar StateGenM

sampleReaderRVarT :: (StatefulGen g m, MonadReader g m) => RVarT m a -> m a
sampleReaderRVarT rvar = ask >>= runRVarT rvar

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
-- The ability to lift is very important - without it, every 'RVar' would have
-- to either be given access to the full capability of the monad in which it
-- will eventually be sampled (which, incidentally, would also have to be
-- monomorphic so you couldn't sample one 'RVar' in more than one monad)
-- or functions manipulating 'RVar's would have to use higher-ranked
-- types to enforce the same kind of isolation and polymorphism.
{-# INLINE runRVarTWith #-}
runRVarTWith :: forall m n g a . StatefulGen g m =>
           (n :-> m) -> RVarT n a -> g -> m a
runRVarTWith liftN (RVarT m) gen = runPromptT' (\p -> uniformPrimM p gen) liftN m

type f :-> g = forall i. f i -> g i

runPromptT' :: forall m n p r . (Monad n) => (p :-> n) -> (m :-> n) -> F (Sum p m) r -> n r
runPromptT' prm lft e = foldF alg e
  where
    alg :: Sum p m i -> n i
    alg (InL x) = prm x
    alg (InR y) = lft y

{-# INLINE uniformPrimM #-}
uniformPrimM :: StatefulGen g m => Prim t -> g -> m t
uniformPrimM (Prim f) g = uniformDoublePositive01M g >>= return . f

-- |@sampleRVarTWith lift x@ is equivalent to @runRVarTWith lift x 'StdRandom'@.
{-# INLINE sampleReaderRVarTWith #-}
sampleReaderRVarTWith ::
       forall m n a g. (StatefulGen g m, MonadReader g m)
    => (forall t. n t -> m t)
    -> RVarT n a
    -> m a
sampleReaderRVarTWith liftN (RVarT m) = runPromptT' (\p -> ask >>= uniformPrimM p) liftN m

-- |@sampleRVarTWith lift x@ is equivalent to @runRVarTWith lift x 'StdRandom'@.
{-# INLINE sampleStateRVarTWith #-}
sampleStateRVarTWith ::
       forall m n a g. (RandomGen g, MonadState g m)
    => (forall t. n t -> m t)
    -> RVarT n a
    -> m a
sampleStateRVarTWith liftN (RVarT m) = runPromptT' (\p -> uniformPrimM p StateGenM) liftN m

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance Functor n => MonadFree Prim (RVarT n) where
  wrap = RVarT . wrap . InL . fmap unRVarT

instance T.MonadTrans RVarT where
  lift = RVarT . wrap . InR . fmap unRVarT . fmap return

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

data RGen = RGen

instance Functor m => StatefulGen RGen (RVarT m) where
    uniformWord32 RGen            = liftF (Prim f)
      where
        f x = floor $ x * fromIntegral (maxBound :: Word32)
    {-# INLINE uniformWord32 #-}
    uniformShortByteString _n RGen = RVarT $ error "ShortByteString"
    {-# INLINE uniformShortByteString #-}

uniformRVarT :: (Functor m, Uniform a) => RVarT m a
uniformRVarT = uniformM RGen
{-# INLINE uniformRVarT #-}

uniformRangeRVarT :: (Functor m, UniformRange a) => (a, a) -> RVarT m a
uniformRangeRVarT r = uniformRM r RGen
{-# INLINE uniformRangeRVarT #-}
