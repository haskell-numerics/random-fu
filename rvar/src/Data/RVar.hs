{-
 -      ``Data/Random/RVar''
 -}
{-# LANGUAGE
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleInstances, 
    GADTs
  #-}

-- |Random variables.  An 'RVar' is a sampleable random variable.  Because
-- probability distributions form a monad, they are quite easy to work with
-- in the standard Haskell monadic styles.  For examples, see the source for
-- any of the 'Distribution' instances - they all are defined in terms of
-- 'RVar's.
module Data.RVar
    ( RVar
    , runRVar
    , RVarT
    , runRVarT
    ) where


import Data.RVar.Internal.Primitives

import qualified Control.Monad.Trans.Class as T
import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Prompt (MonadPrompt(..), PromptT, runPromptT)
import qualified Control.Monad.IO.Class as T
import qualified Control.Monad.Trans as MTL
import qualified Control.Monad.Identity as MTL
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
-- > sampleFrom DevRandom (uniform 1 100) :: IO Int
-- 
-- * In a monad, using a 'MonadRandom' instance:
--
-- > sample (uniform 1 100) :: State PureMT Int
-- 
-- * As a pure function transforming a functional RNG:
-- 
-- > sampleState (uniform 1 100) :: StdGen -> (Int, StdGen)
type RVar = RVarT T.Identity

-- |\"Run\" an 'RVar' - samples the random variable from the provided
-- source of entropy.  Typically 'sample', 'sampleFrom' or 'sampleState' will
-- be more convenient to use.
runRVar :: Monad m => RVar a -> (forall t. Prim t -> m t) -> m a
runRVar x = runRVarT x (return . T.runIdentity)

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
-- To run the random walk, it must first be initialized, and then it can be sampled as usual:
--
-- > do
-- >     rw <- rwalkIO
-- >     x <- sampleFrom DevURandom rw
-- >     y <- sampleFrom DevURandom rw
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
-- Invocation is straightforward (although a bit noisy) if you're used 
-- to MTL, but there is a gotcha lurking here: @sample@ and 'runRVarT' 
-- inherit the extreme generality of 'lift', so there will almost always
-- need to be an explicit type signature lurking somewhere in any client 
-- code making use of 'RVarT' with MTL types.  In this example, the 
-- inferred type of @start@ would be too general to be practical, so the
-- signature for @rwalk@  explicitly fixes it to 'Double'.  Alternatively, 
-- in this case @sample@ could be replaced with
-- @\\x -> runRVarTWith MTL.lift x StdRandom@.
-- 
-- > rwalk :: Int -> Double -> StdGen -> ([Double], StdGen)
-- > rwalk count start gen = evalState (runStateT (sample (replicateM count rwalkState)) gen) start
newtype RVarT m a = RVarT { unRVarT :: PromptT Prim m a }

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
-- The second argument evaluates a \"primitive\" random variate.
{-# INLINE runRVarT #-}
runRVarT :: Monad m => RVarT n a -> (forall t. n t -> m t) -> (forall t. Prim t -> m t) -> m a
runRVarT (RVarT m) liftN liftP = runPromptT return bindP bindN m
    where
        bindP prim cont = liftP prim >>= cont
        bindN nExp cont = liftN nExp >>= cont

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    fail s   = RVarT (fail s)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance MonadPrompt Prim (RVarT n) where
    prompt = RVarT . prompt

instance T.MonadTrans RVarT where
    lift m = RVarT (MTL.lift m)

instance MTL.MonadTrans RVarT where
    lift m = RVarT (MTL.lift m)

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

instance MTL.MonadIO m => MTL.MonadIO (RVarT m) where
    liftIO = MTL.lift . MTL.liftIO

-- I would really like to be able to do this, but I can't because of the
-- blasted Eq and Show in Num's class context...
-- instance (Applicative m, Num a) => Num (RVarT m a) where
--     (+) = liftA2 (+)
--     (-) = liftA2 (-)
--     (*) = liftA2 (*)
--     negate = liftA negate
--     signum = liftA signum
--     abs = liftA abs
--     fromInteger = pure . fromInteger
