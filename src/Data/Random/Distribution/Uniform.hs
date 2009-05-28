{-
 -      ``Data/Random/Distribution/Uniform''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances, 
    UndecidableInstances, EmptyDataDecls,
    TemplateHaskell
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
    
    , realStdUniformCDF
    , realUniformCDF
    ) where

import Data.Random.Internal.TH
import Data.Random.Internal.Words

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

import Data.Word
import Data.Int
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

integralUniformCDF a b x
    | b < a     = integralUniformCDF b a x
    | x < a     = 0
    | x > b     = 1
    | otherwise = (fromIntegral x - fromIntegral a) / (fromIntegral b - fromIntegral a)

bytesNeeded x = case findIndex (> x) powersOf256 of
    Just x -> x
powersOf256 = iterate (256 *) 1

boundedStdUniform :: (Distribution Uniform a, Bounded a) => RVar a
boundedStdUniform = uniform minBound maxBound

boundedStdUniformCDF :: (CDF Uniform a, Bounded a) => a -> Double
boundedStdUniformCDF = cdf (Uniform minBound maxBound)

boundedEnumStdUniform :: (Enum a, Bounded a) => RVar a
boundedEnumStdUniform = enumUniform minBound maxBound

boundedEnumStdUniformCDF :: (Enum a, Bounded a, Ord a) => a -> Double
boundedEnumStdUniformCDF = enumUniformCDF minBound maxBound

floatStdUniform :: RVar Float
floatStdUniform = do
    x <- getRandomWord
    return (wordToFloat x)

doubleStdUniform :: RVar Double
doubleStdUniform = getRandomDouble

realFloatStdUniform :: RealFloat a => RVar a
realFloatStdUniform = do
    let (b, e) = decodeFloat one
    
    x <- uniform 0 (b-1)
    if x == 0
        then return (0 `asTypeOf` one)
        else return (encodeFloat x e)
    
    where one = 1

realStdUniformCDF :: Real a => a -> Double
realStdUniformCDF x
    | x <= 0    = 0
    | x >= 1    = 1
    | otherwise = realToFrac x

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

realUniformCDF :: Real a => a -> a -> a -> Double
realUniformCDF a b x
    | b < a     = realUniformCDF b a x
    | x <= a    = 0
    | x >= b    = 1
    | otherwise = realToFrac (x-a) / realToFrac (b-a)

enumUniform :: Enum a => a -> a -> RVar a
enumUniform a b = do
    x <- integralUniform (fromEnum a) (fromEnum b)
    return (toEnum x)

enumUniformCDF :: (Enum a, Ord a) => a -> a -> a -> Double
enumUniformCDF a b x
    | b < a     = enumUniformCDF b a x
    | x <= a    = 0
    | x >= b    = 1
    | otherwise = (e2f x - e2f a) / (e2f b - e2f a)
    
    where e2f = fromIntegral . fromEnum

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

$( replicateInstances ''Int integralTypes [d|
        instance Distribution Uniform Int   where rvar (Uniform a b) = integralUniform a b
        instance CDF Uniform Int            where cdf  (Uniform a b) = integralUniformCDF a b
    |])

-- Some integral types have specialized StdUniform rvars:
instance Distribution StdUniform Int8       where rvar StdUniform = fmap fromIntegral getRandomByte
instance Distribution StdUniform Word8      where rvar StdUniform = getRandomByte
instance Distribution StdUniform Word64     where rvar StdUniform = getRandomWord
-- and Integer has none...
$( replicateInstances ''Int (integralTypes \\ [''Int8, ''Word8, ''Word64, ''Integer]) [d|
        instance Distribution StdUniform Int    where rvar StdUniform = fmap fromIntegral getRandomWord
    |])

$( replicateInstances ''Int (integralTypes \\ [''Integer]) [d|
        instance CDF StdUniform Int         where cdf  StdUniform = boundedStdUniformCDF
    |])


instance Distribution Uniform Float         where rvar (Uniform a b) = floatUniform  a b
instance Distribution Uniform Double        where rvar (Uniform a b) = doubleUniform a b
instance CDF Uniform Float                  where cdf  (Uniform a b) = realUniformCDF a b
instance CDF Uniform Double                 where cdf  (Uniform a b) = realUniformCDF a b

instance Distribution StdUniform Float      where rvar StdUniform = floatStdUniform
instance Distribution StdUniform Double     where rvar StdUniform = doubleStdUniform
instance CDF StdUniform Float               where cdf  StdUniform = realStdUniformCDF
instance CDF StdUniform Double              where cdf  StdUniform = realStdUniformCDF

instance Distribution Uniform ()            where rvar (Uniform a b) = return ()
instance CDF Uniform ()                     where cdf  (Uniform a b) = return 1
$( replicateInstances ''Char [''Char, ''Bool, ''Ordering] [d|
        instance Distribution Uniform Char  where rvar (Uniform a b) = enumUniform a b
        instance CDF Uniform Char           where cdf  (Uniform a b) = enumUniformCDF a b

    |])

instance Distribution StdUniform ()         where rvar StdUniform = return ()
instance CDF StdUniform ()                  where cdf  StdUniform = return 1
instance Distribution StdUniform Bool       where rvar StdUniform = fmap even getRandomByte
instance CDF StdUniform Bool                where cdf  StdUniform = boundedEnumStdUniformCDF

instance Distribution StdUniform Char       where rvar StdUniform = boundedEnumStdUniform
instance CDF StdUniform Char                where cdf  StdUniform = boundedEnumStdUniformCDF
instance Distribution StdUniform Ordering   where rvar StdUniform = boundedEnumStdUniform
instance CDF StdUniform Ordering            where cdf  StdUniform = boundedEnumStdUniformCDF

