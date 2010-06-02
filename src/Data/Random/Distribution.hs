{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}
module Data.Random.Distribution where

import Data.Random.Lift
import Data.Random.RVar

-- |A 'Distribution' is a data representation of a random variable's probability
-- structure.  For example, in "Data.Random.Distribution.Normal", the 'Normal'
-- distribution is defined as:
--
-- > data Normal a
-- >     = StdNormal
-- >     | Normal a a
-- 
-- Where the two parameters of the 'Normal' data constructor are the mean and
-- standard deviation of the random variable, respectively.  To make use of
-- the 'Normal' type, one can convert it to an 'rvar' and manipulate it or
-- sample it directly:
--
-- > x <- sample (rvar (Normal 10 2))
-- > x <- sample (Normal 10 2)
-- 
-- A 'Distribution' is typically more transparent than an 'RVar'
-- but less composable (precisely because of that transparency).  There are 
-- several practical uses for types implementing 'Distribution':
-- 
-- * Typically, a 'Distribution' will expose several parameters of a standard 
-- mathematical model of a probability distribution, such as mean and std deviation for
-- the normal distribution.  Thus, they can be manipulated analytically using
-- mathematical insights about the distributions they represent.  For example,
-- a collection of bernoulli variables could be simplified into a (hopefully) smaller
-- collection of binomial variables.
-- 
-- * Because they are generally just containers for parameters, they can be
-- easily serialized to persistent storage or read from user-supplied 
-- configurations (eg, initialization data for a simulation).
-- 
-- * If a type additionally implements the 'CDF' subclass, which extends
-- 'Distribution' with a cumulative density function, an arbitrary random
-- variable 'x' can be tested against the distribution by testing
-- @fmap (cdf dist) x@ for uniformity.
-- 
-- On the other hand, most 'Distribution's will not be closed under all the
-- same operations as 'RVar' (which, being a monad, allows lots of very
-- useful operations to be applied).  The sum of two uniformly-distributed 
-- variables, for example, is not uniformly distributed.  To support general 
-- composition, the 'Distribution' class defines a function 'rvar' to 
-- construct the more-abstract and more-composable 'RVar' representation 
-- of a random variable.
class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: d t -> RVar t
    rvar = rvarT
    
    -- |Return a random variable with the given distribution, pre-lifted to an arbitrary 'RVarT'.
    -- Any arbitrary 'RVar' can also be converted to an 'RVarT m' for an arbitrary 'm', using
    -- either 'lift' or 'sample'.
    rvarT :: d t -> RVarT n t
    rvarT d = lift (rvar d)


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
