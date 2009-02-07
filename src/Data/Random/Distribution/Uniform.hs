{-
 -      ``Data/Random/Distribution/Uniform''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances, 
    UndecidableInstances
  #-}

module Data.Random.Distribution.Uniform
    ( Uniform(..)
	, UniformByClassification(..)
	, uniform
	
    , StdUniform(..)
    , StdUniformByClassification(..)
    , stdUniform
    
    , integralUniform
    , realFloatUniform
    
    , boundedStdUniform
    , boundedEnumStdUniform
    , realFloatStdUniform
    ) where

import Data.Random.Internal.Classification

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Control.Monad.Loops

integralUniform a b
    | a > b     = compute b a
    | otherwise = compute a b
    where
        compute a b = do
            let m = 1 + toInteger b - toInteger a
            
            let bytes = bytesNeeded m
                maxXpossible = (powersOf256 !! bytes) - 1
            
            x <- iterateUntil (maxXpossible - maxXpossible `mod` m >) (nByteInteger bytes)
            return (a + fromInteger (x `mod` m))


bytesNeeded x = case findIndex (> x) powersOf256 of
    Just x -> x
powersOf256 = iterate (256 *) 1

boundedStdUniform :: (Distribution Uniform a, Bounded a) => RVar a
boundedStdUniform = uniform minBound maxBound

boundedEnumStdUniform :: (Enum a, Bounded a) => RVar a
boundedEnumStdUniform = enumUniform minBound maxBound

-- (0,1]
realFloatStdUniform :: RealFloat a => RVar a
realFloatStdUniform | False     = return one
                    | otherwise = do
    let bitsNeeded  = floatDigits one
        (_, e) = decodeFloat one
    
    x <- nBitInteger bitsNeeded
    if x == 0
        then return 1
        else return (encodeFloat x (e-1))
    
    where one = 1

realFloatUniform :: RealFloat a => a -> a -> RVar a
realFloatUniform 0 1 = realFloatStdUniform
realFloatUniform a b = do
    x <- realFloatStdUniform
    return (a + x * (b - a))

enumUniform :: Enum a => a -> a -> RVar a
enumUniform a b = do
    x <- integralUniform (fromEnum a) (fromEnum b)
    return (toEnum x)

uniform :: Distribution Uniform a => a -> a -> RVar a
uniform a b = rvar (Uniform a b)

stdUniform :: Distribution StdUniform a => RVar a
stdUniform = rvar StdUniform

class (Classification NumericType t c) => UniformByClassification c t where
    uniformByClassification :: t -> t -> RVar t

class (Classification NumericType t c) => StdUniformByClassification c t where
    stdUniformByClassification :: RVar t

data Uniform t = Uniform !t !t
data StdUniform t = StdUniform

instance UniformByClassification c t => Distribution Uniform t
    where rvar (Uniform a b) = uniformByClassification a b

instance StdUniformByClassification c t => Distribution StdUniform t
    where rvar _ = stdUniformByClassification

instance (Classification NumericType t IntegralType, Integral t) => UniformByClassification IntegralType t
    where uniformByClassification = integralUniform
instance (Classification NumericType t FractionalType, RealFloat t) => UniformByClassification FractionalType t
    where uniformByClassification = realFloatUniform
instance (Classification NumericType t EnumType, Enum t) => UniformByClassification EnumType t
    where uniformByClassification = enumUniform

instance (Classification NumericType t IntegralType, Integral t, Bounded t) => StdUniformByClassification IntegralType t
    where stdUniformByClassification = boundedStdUniform
instance (Classification NumericType t FractionalType, RealFloat t) => StdUniformByClassification FractionalType t
    where stdUniformByClassification = realFloatStdUniform
instance (Classification NumericType t EnumType, Enum t, Bounded t) => StdUniformByClassification EnumType t
    where stdUniformByClassification = boundedStdUniform
