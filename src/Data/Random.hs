{-# LANGUAGE CPP #-}

-- |Flexible modeling and sampling of random variables.
--
-- The central abstraction in this library is the concept of a random 
-- variable.  It is not fully formalized in the standard measure-theoretic 
-- language, but rather is informally defined as a \"thing you can get random 
-- values out of\".  Different random variables may have different types of 
-- values they can return or the same types but different probabilities for
-- each value they can return.  The random values you get out of them are
-- traditionally called \"random variates\".
-- 
-- Most imperative-language random number libraries are all about obtaining 
-- and manipulating random variates.  This one is about defining, manipulating 
-- and sampling random variables.  Computationally, the distinction is small 
-- and mostly just a matter of perspective, but from a program design 
-- perspective it serves as both a powerfully composable abstraction and a
-- very useful separation of concerns.
-- 
-- Abstract random variables as implemented by 'RVar' are composable.  They can
-- be defined in a monadic / \"imperative\" style that amounts to manipulating
-- variates, but with strict type-level isolation.  Concrete random variables
-- are also provided, but they do not compose as generically.  The 'Distribution'
-- type class allows concrete random variables to be abstracted so that they can
-- be composed.  For examples of both, see the documentation for 'RVar' and
-- 'Distribution', as well as the code for any of the concrete distributions
-- such as 'Uniform', 'Gamma', etc.
-- 
-- Both abstract and concrete random variables can be sampled (despite the
-- types Haddock may list for the functions) by the functions in
-- "Data.Random.Sample".
-- 
-- Random variable sampling is done with regard to a generic basis of primitive
-- random variables defined in "Data.Random.Internal.Primitives".  The actual
-- set of primitives is still fairly experimental, which is why it is in the
-- \"Internal\" sub-heirarchy.  "Data.Random.Source" defines classes for entropy
-- sources that provide implementations of these primitive variables.  Several
-- implementations are available in the Data.Random.Source.* modules.
-- 
-- "Data.Random.Distribution" exports the typeclasses for describing distributions
-- (concrete random variables), and Data.Random.Distribution.* export various
-- specific distributions.  A 'Distribution' is a data representation of a 
-- random variable with some regular structure - for example, a uniformly 
-- distributed variable can be represented by the 'Uniform' data type, which
-- models such variables by the lower and upper bounds of their ranges.
--
-- "Data.Random.RVar" exports the 'RVar' type, which is a probability distribution
-- monad that allows for concise definitions and flexible sampling of abstract random
-- variables.
module Data.Random
    ( module Data.Random.Sample
    , module Data.Random.Source
-- TODO: make /dev/random support part of its own package so that
-- dependencies on it must be made explicit
#ifndef windows
    , module Data.Random.Source.DevRandom
#endif
    , module Data.Random.Source.StdGen
    , module Data.Random.Source.PureMT
    , module Data.Random.Source.Std
    , module Data.Random.Distribution
    , module Data.Random.Distribution.Bernoulli
    , module Data.Random.Distribution.Beta
    , module Data.Random.Distribution.Binomial
    , module Data.Random.Distribution.Categorical
    , module Data.Random.Distribution.Dirichlet
    , module Data.Random.Distribution.Gamma
    , module Data.Random.Distribution.Exponential
    , module Data.Random.Distribution.Multinomial
    , module Data.Random.Distribution.Normal
    , module Data.Random.Distribution.Poisson
    , module Data.Random.Distribution.Rayleigh
    , module Data.Random.Distribution.Triangular
    , module Data.Random.Distribution.Uniform
    , module Data.Random.Distribution.Ziggurat
    , module Data.Random.List
    , module Data.Random.RVar
    ) where

import Data.Random.Sample
import Data.Random.Source (MonadRandom, RandomSource)
#ifndef windows
import Data.Random.Source.DevRandom
#endif
import Data.Random.Source.MWC ()
import Data.Random.Source.StdGen
import Data.Random.Source.PureMT
import Data.Random.Source.Std
import Data.Random.Distribution
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Categorical
import Data.Random.Distribution.Dirichlet
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Multinomial
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Rayleigh
import Data.Random.Distribution.Triangular
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Ziggurat
import Data.Random.Lift ()
import Data.Random.List
import Data.Random.RVar

