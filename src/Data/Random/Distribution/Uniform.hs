{-
 -      ``Data/Random/Distribution/Uniform''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts, PatternSignatures, RankNTypes
  #-}

module Data.Random.Distribution.Uniform
    ( Uniform(..)
	, uniform
	
    , StdUniform(..)
    , stdUniform
    
    , integralUniform
    , realFloatUniform
    
    , boundedStdUniform
    , boundedEnumStdUniform
    , realFloatStdUniform
    ) where

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

data Uniform t = Uniform !t !t
data StdUniform t = StdUniform

uniform :: Distribution Uniform a => a -> a -> RVar a
uniform a b = rvar (Uniform a b)

stdUniform :: Distribution StdUniform a => RVar a
stdUniform = rvar StdUniform

instance Distribution Uniform Int8          where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int16         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int32         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int64         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Int           where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Integer       where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word8         where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word16        where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word32        where rvar (Uniform a b) = integralUniform a b
instance Distribution Uniform Word64        where rvar (Uniform a b) = integralUniform a b

instance Distribution StdUniform Int8       where rvar _ = boundedStdUniform
instance Distribution StdUniform Int16      where rvar _ = boundedStdUniform
instance Distribution StdUniform Int32      where rvar _ = boundedStdUniform
instance Distribution StdUniform Int64      where rvar _ = boundedStdUniform
instance Distribution StdUniform Int        where rvar _ = boundedStdUniform
instance Distribution StdUniform Word8      where rvar _ = boundedStdUniform
instance Distribution StdUniform Word16     where rvar _ = boundedStdUniform
instance Distribution StdUniform Word32     where rvar _ = boundedStdUniform
instance Distribution StdUniform Word64     where rvar _ = boundedStdUniform

instance Distribution Uniform Float         where rvar (Uniform a b) = realFloatUniform a b
instance Distribution Uniform Double        where rvar (Uniform a b) = realFloatUniform a b

instance Distribution StdUniform Float      where rvar _ = realFloatStdUniform
instance Distribution StdUniform Double     where rvar _ = realFloatStdUniform

instance Distribution Uniform Bool          where rvar (Uniform a b) = enumUniform a b
instance Distribution Uniform Char          where rvar (Uniform a b) = enumUniform a b
instance Distribution Uniform Ordering      where rvar (Uniform a b) = enumUniform a b

instance Distribution StdUniform Bool       where rvar _ = getRandomBytes 1 >>= (\(x:_) -> return (x >= 128))
instance Distribution StdUniform Char       where rvar _ = boundedEnumStdUniform
instance Distribution StdUniform Ordering   where rvar _ = boundedEnumStdUniform

instance Distribution Uniform ()            where rvar _ = return ()
