{-
 -      ``Data/Random/Distribution/Uniform''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances, 
    UndecidableInstances, EmptyDataDecls
  #-}

module Data.Random.Distribution.Uniform
    ( Uniform(..)
	, UniformByClassification(..)
	, uniform
	
    , StdUniform(..)
    , StdUniformByClassification(..)
    , stdUniform
    
    , UniformType
    
    , integralUniform
    , realFloatUniform
    , floatUniform
    , doubleUniform
    
    , boundedStdUniform
    , boundedEnumStdUniform
    , realFloatStdUniform
    , floatStdUniform
    , doubleStdUniform
    ) where

import Data.Random.Internal.Classification

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Control.Monad.Loops

integralUniform :: (Integral a) => a -> a -> RVar a
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
floatStdUniform :: RVar Float
floatStdUniform = do
    x <- getRandomWord
    if x == 0
        then return 1
        else return ((encodeFloat $! toInteger (x `shiftR` (64-23))) $ (-23))

doubleStdUniform :: RVar Double
doubleStdUniform = do
    x <- getRandomWord
    if x == 0
        then return 1
        else return ((encodeFloat $! toInteger (x `shiftR` (64-52))) $ (-52))

realFloatStdUniform :: RealFloat a => RVar a
realFloatStdUniform = do
    let bitsNeeded  = floatDigits one
        (_, e) = decodeFloat one
    
    x <- nBitInteger bitsNeeded
    if x == 0
        then return one
        else return (encodeFloat x (e-1))
    
    where one = 1

floatUniform :: Float -> Float -> RVar Float
floatUniform 0 1 = floatStdUniform
floatUniform a b = do
    x <- floatStdUniform
    return (a + x * (b - a))

doubleUniform :: Double -> Double -> RVar Double
doubleUniform 0 1 = doubleStdUniform
doubleUniform a b = do
    x <- doubleStdUniform
    return (a + x * (b - a))

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

stdUniform :: (Distribution StdUniform a) => RVar a
stdUniform = rvar StdUniform

class (Classification UniformType t c) => UniformByClassification c t where
    uniformByClassification :: t -> t -> RVar t

class (Classification UniformType t c) => StdUniformByClassification c t where
    stdUniformByClassification :: RVar t

data Uniform t = Uniform !t !t
data StdUniform t = StdUniform

instance UniformByClassification c t => Distribution Uniform t
    where rvar (Uniform a b) = uniformByClassification a b

instance StdUniformByClassification c t => Distribution StdUniform t
    where rvar _ = stdUniformByClassification

instance (Classification UniformType t IntegralType, Integral t) => UniformByClassification IntegralType t
    where uniformByClassification = integralUniform
instance UniformByClassification Float Float
    where uniformByClassification = floatUniform
instance UniformByClassification Double Double
    where uniformByClassification = doubleUniform
instance (Classification UniformType t FractionalType, RealFloat t) => UniformByClassification FractionalType t
    where uniformByClassification = realFloatUniform
instance (Classification UniformType t EnumType, Enum t) => UniformByClassification EnumType t
    where uniformByClassification = enumUniform

instance (Classification UniformType t IntegralType, Integral t, Bounded t) => StdUniformByClassification IntegralType t
    where stdUniformByClassification = boundedStdUniform
instance StdUniformByClassification Float Float
    where stdUniformByClassification = floatStdUniform
instance StdUniformByClassification Double Double
    where stdUniformByClassification = doubleStdUniform
instance (Classification UniformType t FractionalType, RealFloat t) => StdUniformByClassification FractionalType t
    where stdUniformByClassification = realFloatStdUniform
instance (Classification UniformType t EnumType, Enum t, Bounded t) => StdUniformByClassification EnumType t
    where stdUniformByClassification = boundedStdUniform

data UniformType

instance Classification UniformType Int            IntegralType
instance Classification UniformType Int8           IntegralType
instance Classification UniformType Int16          IntegralType
instance Classification UniformType Int32          IntegralType
instance Classification UniformType Int64          IntegralType
instance Classification UniformType Word8          IntegralType
instance Classification UniformType Word16         IntegralType
instance Classification UniformType Word32         IntegralType
instance Classification UniformType Word64         IntegralType
instance Classification UniformType Integer        IntegralType

instance Classification UniformType Float          Float
instance Classification UniformType Double         Double
instance Classification UniformType (Ratio a)      FractionalType

instance Classification UniformType Char           EnumType
instance Classification UniformType Bool           EnumType
instance Classification UniformType ()             EnumType
instance Classification UniformType Ordering       EnumType