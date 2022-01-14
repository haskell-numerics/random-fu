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
-- perspective it provides both a powerfully composable abstraction and a
-- very useful separation of concerns.
--
-- Abstract random variables as implemented by 'RVar' are composable.  They can
-- be defined in a monadic / \"imperative\" style that amounts to manipulating
-- variates, but with strict type-level isolation.  Concrete random variables
-- are also provided, but they do not compose as generically.  The 'Distribution'
-- type class allows concrete random variables to \"forget\" their concreteness
-- so that they can be composed.  For examples of both, see the documentation
-- for 'RVar' and 'Distribution', as well as the code for any of the concrete
-- distributions such as 'Uniform', 'Gamma', etc.
--
-- Both abstract and concrete random variables can be sampled (despite the
-- types GHCi may list for the functions) by the functions in "Data.Random.Sample".
--
-- Random variable sampling is done with regard to a generic basis of primitive
-- random variables defined in "Data.Random.Internal.Primitives".  This basis
-- is very low-level and the actual set of primitives is still fairly experimental,
-- which is why it is in the \"Internal\" sub-heirarchy.  User-defined variables
-- should use the existing high-level variables such as 'Uniform' and 'Normal'
-- rather than these basis variables.  "Data.Random.Source" defines classes for
-- entropy sources that provide implementations of these primitive variables.
-- Several implementations are available in the Data.Random.Source.* modules.
module Data.Random
    ( -- * Random variables
      -- ** Abstract ('RVar')
      RVar, RVarT,
      runRVar, runRVarT, runRVarTWith,

      -- ** Concrete ('Distribution')
      Distribution(..), CDF(..), PDF(..),

      -- * Sampling random variables
      Sampleable(..), sample, sampleState, samplePure,

      -- * A few very common distributions
      Uniform(..), uniform, uniformT,
      StdUniform(..), stdUniform, stdUniformT,
      Normal(..), normal, stdNormal, normalT, stdNormalT,
      Gamma(..), gamma, gammaT,

      -- * Entropy Sources
      StatefulGen, RandomGen,

      -- * Useful list-based operations
      randomElement,
      shuffle, shuffleN, shuffleNofM

    ) where

import Data.Random.Sample
import Data.Random.Distribution
import Data.Random.Distribution.Gamma
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform

import Data.Random.Lift ()
import Data.Random.List
import Data.Random.RVar

import System.Random.Stateful (StatefulGen, RandomGen)
