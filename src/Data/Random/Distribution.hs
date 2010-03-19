{-
 -      ``Data/Random/Distribution''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution where

import Data.Random.Lift
import Data.Random.RVar

import Control.Monad
import qualified Data.Vector.Generic as V
-- import qualified Data.Vector.Fusion.Stream.Monadic as S

-- |A definition of a random variable's distribution.  From the distribution
-- an 'RVar' can be created, or the distribution can be directly sampled using 
-- 'sampleFrom' or 'sample'.
class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: d t -> RVar t
    
    -- |Return a random vector with N i.i.d. variables of this distribution
    rvec :: V.Vector v t => Int -> d t -> RVar (v t)
        -- I can't find any indication of what the "right way" might be to
        -- implement essentially "replicateM" for a vector.  I can't seem
        -- to find a function @S.Stream m a -> m (v a)@ either.
    rvec n d = do
        list <- replicateM n (rvar d)
        return (V.fromList list)
        -- = V.unstreamM (S.generateM n (\_ -> rvar d))
        -- (best surrogate i've come up with so far for an "unstreamM" 
        -- which doesn't seem to exist is S.foldr V.cons V.empty)

class Distribution d t => CDF d t where
    -- |Return the cumulative distribution function of this distribution.
    -- That is, a function taking @x :: t@ to the probability that the next
    -- sample will return a value less than or equal to x, according to some
    -- order or partial order (not necessarily an obvious one).
    --
    -- In the case where 't' is an instance of Ord, 'cdf' should correspond
    -- to the CDF with respect to that order.
    -- 
    -- In other cases, 'cdf' is only required to satisfy the following law:
    -- @fmap (cdf d) (rvar d)@
    -- must be uniformly distributed over (0,1).  Inclusion of either endpoint is optional,
    -- though the preferred range is (0,1].
    -- 
    -- Thus, 'cdf' for a product type should not be a joint CDF as commonly 
    -- defined, as that definition violates both conditions.
    -- Instead, it should be a univariate CDF over the product type.  That is,
    -- it should represent the CDF with respect to the lexicographic order
    -- of the tuple.
    cdf :: d t -> t -> Double

-- |Return a random variable with the given distribution, pre-lifted to an arbitrary 'RVarT'.
-- Any arbitrary 'RVar' can also be converted to an 'RVarT m' for an arbitrary 'm', using
-- either 'lift' or 'sample'.
rvarT :: Distribution d t => d t -> RVarT n t
rvarT d = lift (rvar d)

