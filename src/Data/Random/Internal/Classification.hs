{-
 -      ``Data/Random/Internal/Classification''
 -      
 -      "Classification systems" - for a motivating example, see
 -  the implementation of the Uniform distribution.  Basically,
 -  I would like to make instances like:
 -  
 -  > instance RealFloat a => Distribution Uniform a where ...
 -  > instance Integral a =>  Distribution Uniform a where ...
 -  
 -  and so on.  However, this is not sound - what happens if someone
 -  comes along and makes a type that's an instance of both Integral and
 -  RealFloat?
 -  
 -  So, we introduce a classification system based on phantom types, so
 -  that each type can be unambiguously declared to be "intensionally"
 -  Integral, Floating, or whatever.
 -  
 -  Now, obviously it'd be nice not to clutter the Distribution typeclass
 -  with extra phantom types that the end user shouldn't care about.  Hence
 -  the pattern of introducing typeclasses such as "UniformByClassification"
 -  
 -  Now, if a new type comes along that is Integral, a single declaration
 -  of the following form suffices to attach it to all such Distribution
 -  instances:
 -  
 -  > instance Classification NumericType t IntegralType
 -  
 -  Not quite as automagic as the "Integral a => Distribution ..."  case,
 -  but a bit closer.  Not only that, it leaves open the possibility that
 -  a user may bring in a type that is "mostly" integral, and has an Integral
 -  instance, but should be handled differently for purposes of uniform
 -  random number generation.  In such a case, the user may introduce a new
 -  classification of their own and provide the required instances for that
 -  classification.
 -  
 -  All in all, although it is not yet well-tested, it has the "feel" of 
 -  a good compromise.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    EmptyDataDecls
  #-}


module Data.Random.Internal.Classification where

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex

-- classificiation system, experimental
--      c (a phantom type) is the classification system
--      t is the type to be classified
--      tc (a phantom type) is the classification of t according to c
class Classification c t tc | c t -> tc

-- NumericType : a simple classification system covering the cases we care
-- about when sampling distributions
data NumericType

data IntegralType
data FractionalType
data EnumType

instance Classification NumericType Int            IntegralType
instance Classification NumericType Int8           IntegralType
instance Classification NumericType Int16          IntegralType
instance Classification NumericType Int32          IntegralType
instance Classification NumericType Int64          IntegralType
instance Classification NumericType Word8          IntegralType
instance Classification NumericType Word16         IntegralType
instance Classification NumericType Word32         IntegralType
instance Classification NumericType Word64         IntegralType
instance Classification NumericType Integer        IntegralType

instance Classification NumericType Float          FractionalType
instance Classification NumericType Double         FractionalType
instance Classification NumericType (Ratio a)      FractionalType
instance Classification NumericType (Complex a)    FractionalType

instance Classification NumericType Char           EnumType
instance Classification NumericType Bool           EnumType
instance Classification NumericType ()             EnumType
instance Classification NumericType Ordering       EnumType