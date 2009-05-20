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
	, uniform
	
    , StdUniform(..)
    , stdUniform
    
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

import Data.Random.Internal.Words
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

floatStdUniform :: RVar Float
floatStdUniform = do
    x <- getRandomWord
    return (wordToFloat x)

doubleStdUniform :: RVar Double
doubleStdUniform = getRandomDouble

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

-- |Get a \"standard\" uniformly distributed value.
-- For integral types, this means uniformly distributed over the full range
-- of the type (and hence there is no support for Integer).  For fractional
-- types, this means uniformly distributed on the interval [0,1).
stdUniform :: (Distribution StdUniform a) => RVar a
stdUniform = rvar StdUniform

stdUniformPos :: (Distribution StdUniform a, Ord a, Num a) => RVar a
stdUniformPos = do
    x <- stdUniform
    if x > 0
        then return x
        else stdUniformPos

data Uniform t = Uniform !t !t
data StdUniform t = StdUniform

instance Distribution Uniform Int           where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int8          where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int16         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int32         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int64         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word8         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word16        where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word32        where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word64        where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Integer       where rvar (Uniform a b) = integralUniform a b

instance Distribution Uniform Float         where rvar (Uniform a b) = floatUniform  a b
instance Distribution Uniform Double        where rvar (Uniform a b) = doubleUniform a b

instance Distribution Uniform Char          where rvar (Uniform a b) = enumUniform a b
instance Distribution Uniform Bool          where rvar (Uniform a b) = enumUniform a b
instance Distribution Uniform ()            where rvar (Uniform a b) = enumUniform a b
instance Distribution Uniform Ordering      where rvar (Uniform a b) = enumUniform a b

instance Distribution StdUniform Int        where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Int8       where rvar StdUniform = fmap fromIntegral getRandomByte
instance Distribution StdUniform Int16      where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Int32      where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Int64      where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Word8      where rvar StdUniform = getRandomByte
instance Distribution StdUniform Word16     where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Word32     where rvar StdUniform = fmap fromIntegral getRandomWord
instance Distribution StdUniform Word64     where rvar StdUniform = fmap fromIntegral getRandomWord

instance Distribution StdUniform Float      where rvar StdUniform = floatStdUniform
instance Distribution StdUniform Double     where rvar StdUniform = doubleStdUniform

instance Distribution StdUniform Char       where rvar StdUniform = boundedEnumStdUniform
instance Distribution StdUniform Bool       where rvar StdUniform = fmap even getRandomByte
instance Distribution StdUniform ()         where rvar StdUniform = return ()
instance Distribution StdUniform Ordering   where rvar StdUniform = boundedEnumStdUniform

