{-
 -      ``Data/Random/RVar''
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances,
    GADTs,
    ScopedTypeVariables,
    CPP
  #-}

-- |Random variables.  An 'RVar' is a sampleable random variable.  Because
-- probability distributions form a monad, they are quite easy to work with
-- in the standard Haskell monadic styles.  For examples, see the source for
-- any of the 'Distribution' instances - they all are defined in terms of
-- 'RVar's.
module Data.RVar
    ( RandomSource
    , MonadRandom
        ( getRandomWord8
        , getRandomWord16
        , getRandomWord32
        , getRandomWord64
        , getRandomDouble
        , getRandomNByteInteger
        )

    , RVar
    , runRVar, sampleRVar

    , RVarT
    , runRVarT, sampleRVarT
    , runRVarTWith, sampleRVarTWith
    ) where


import Data.Random.Internal.Source (Prim(..), MonadRandom(..), RandomSource(..))
import Data.Random.Source ({-instances-})

import qualified Control.Monad.Trans.Class as T
import Control.Monad (liftM, ap)
import Control.Monad.Prompt (MonadPrompt(..), PromptT, runPromptT)
import qualified Control.Monad.IO.Class as T
import qualified Control.Monad.Trans as MTL
import qualified Data.Functor.Identity as T

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
-- * In a monad, using a 'RandomSource':
--
-- > runRVar (uniform 1 100) DevRandom :: IO Int
--
-- * In a monad, using a 'MonadRandom' instance:
--
-- > sampleRVar (uniform 1 100) :: State PureMT Int
--
-- * As a pure function transforming a functional RNG:
--
-- > sampleState (uniform 1 100) :: StdGen -> (Int, StdGen)
--
-- (where @sampleState = runState . sampleRVar@)
type RVar = RVarT T.Identity

-- |\"Run\" an 'RVar' - samples the random variable from the provided
-- source of entropy.
runRVar :: RandomSource m s => RVar a -> s -> m a
runRVar = runRVarTWith (return . T.runIdentity)

-- |@sampleRVar x@ is equivalent to @runRVar x 'StdRandom'@.
sampleRVar :: MonadRandom m => RVar a -> m a
sampleRVar = sampleRVarTWith (return . T.runIdentity)

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
newtype RVarT m a = RVarT { unRVarT :: PromptT Prim m a }

runRVarT :: RandomSource m s => RVarT m a -> s -> m a
runRVarT = runRVarTWith id

sampleRVarT :: MonadRandom m => RVarT m a -> m a
sampleRVarT = sampleRVarTWith id

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
runRVarTWith :: forall m n s a. RandomSource m s => (forall t. n t -> m t) -> RVarT n a -> s -> m a
runRVarTWith liftN (RVarT m) src = runPromptT return bindP bindN m
    where
        bindP :: forall t. (Prim t -> (t -> m a) -> m a)
        bindP prim cont = getRandomPrimFrom src prim >>= cont

        bindN :: forall t. n t -> (t -> m a) -> m a
        bindN nExp cont = liftN nExp >>= cont

-- |@sampleRVarTWith lift x@ is equivalent to @runRVarTWith lift x 'StdRandom'@.
sampleRVarTWith :: forall m n a. MonadRandom m => (forall t. n t -> m t) -> RVarT n a -> m a
sampleRVarTWith liftN (RVarT m) = runPromptT return bindP bindN m
    where
        bindP :: forall t. (Prim t -> (t -> m a) -> m a)
        bindP prim cont = getRandomPrim prim >>= cont

        bindN :: forall t. n t -> (t -> m a) -> m a
        bindN nExp cont = liftN nExp >>= cont

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance MonadRandom (RVarT n) where
    getRandomPrim = RVarT . prompt

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance MonadPrompt Prim (RVarT n) where
    prompt = RVarT . prompt

instance T.MonadTrans RVarT where
    lift m = RVarT (MTL.lift m)

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

#ifndef MTL2

instance MTL.MonadTrans RVarT where
    lift m = RVarT (MTL.lift m)

instance MTL.MonadIO m => MTL.MonadIO (RVarT m) where
    liftIO = MTL.lift . MTL.liftIO

#endif
