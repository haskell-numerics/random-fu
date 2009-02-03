{-
 -      ``Data/Random/Distribution/Uniform/Internal''
 -      (c) 2009 James Cook
 -}
{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.Random.Distribution.Uniform.Internal
    ( Uniform(..)
    , StdUniform(..)
    
    , computeIntegralUniformDist
    , computeRealFloatUniformDist
    , computeFractionalStdUniformDist
    , computeEnumUniformDist
    , computeBoundedStdUniformDist
    ) where

import Data.Random.Source
import Data.Random.Distribution

import Data.List

import Data.Int
import Data.Word

data Uniform t = Uniform !t !t
data StdUniform t = StdUniform

{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Integer -> m Integer #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Int     -> m Int     #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Int8    -> m Int8    #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Int16   -> m Int16   #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Int32   -> m Int32   #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Int64   -> m Int64   #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Word8   -> m Word8   #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Word16  -> m Word16  #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Word32  -> m Word32  #-}
{-# SPECIALIZE computeIntegralUniformDist :: RandomSource m s => s -> Uniform Word64  -> m Word64  #-}

computeIntegralUniformDist src (Uniform a b)
    | a > b     = compute b a
    | otherwise = compute a b
    where
        compute a b = do
            let m = toInteger b - toInteger a + 1
            
            let bytes = bytesNeeded m
                maxXpossible = (powersOf256 !! bytes) - 1
            
            x <- getNByteIntegerWhere src bytes (\x -> maxXpossible - x > maxXpossible `mod` m)
            return (a + fromInteger (x `mod` m))

bytesNeeded x = case findIndex (> x) powersOf256 of
    Just x -> x
powersOf256 = iterate (256 *) 1


computeBoundedStdUniformDist :: (RandomSource m s, Distribution Uniform a, Bounded a) => s -> StdUniform a -> m a
computeBoundedStdUniformDist src StdUniform = sampleFrom src (Uniform minBound maxBound)

{-# SPECIALIZE computeRealFloatUniformDist :: RandomSource m s => s -> Uniform Float  -> m Float  #-}
{-# SPECIALIZE computeRealFloatUniformDist :: RandomSource m s => s -> Uniform Double -> m Double #-}

-- (0,1]
computeRealFloatUniformDist src (Uniform 0 f@1) = do
    let bitsNeeded  = floatDigits f
        (_, e) = decodeFloat (1 `asTypeOf` f)
    
    x <- getNBitInteger src bitsNeeded
    if x == 0
        then return 1
        else return (encodeFloat x (e-1))
    
computeRealFloatUniformDist src (Uniform a b) = do
    let m = b - a
    x <- computeRealFloatUniformDist src (Uniform 0 1)
    return (a + m * x)

computeFractionalStdUniformDist :: (RandomSource m s, Distribution Uniform a, Num a) => s -> StdUniform a -> m a
computeFractionalStdUniformDist src StdUniform = sampleFrom src (Uniform 0 1)

computeEnumUniformDist :: (RandomSource m s, Enum a) => s -> Uniform a -> m a
computeEnumUniformDist src (Uniform a b) = do
        x <- computeIntegralUniformDist src (Uniform (fromEnum a) (fromEnum b))
        return (toEnum x)

