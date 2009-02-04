{-
 -      ``Data/Random/Internal/Classification''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
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