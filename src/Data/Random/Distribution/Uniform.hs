{-
 -      ``Data/Random/Distribution/Uniform''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution.Uniform where

import Data.Random.Source
import Data.Random.Distribution

import Data.Word
import Data.Int
import Data.Bits
import Data.List

-- quick & dirty test:
-- let x = sampleFrom DevRandom (Uniform (-100) (100) :: Uniform Int8) :: IO Int8 in mapM_ (\x -> putStrLn (replicate (x `div` 10) '*')) . map length . group . sort =<< replicateM 100000 x
data Uniform t = Uniform !t !t

instance Distribution Uniform Int8    where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int16   where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int32   where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int64   where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int     where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Integer where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word8   where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word16  where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word32  where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word64  where sampleFrom = computeIntegralUniformDist
instance (Distribution Uniform a, Distribution Uniform b) => Distribution Uniform (a,b) where
    sampleFrom s (Uniform (a1,b1) (a2,b2)) = do
        a <- sampleFrom s (Uniform a1 a2)
        b <- sampleFrom s (Uniform b1 b2)
        return (a,b)

computeNByteInteger :: RandomSource m s => s -> Int -> m Integer
computeNByteInteger src n = do
    xs <- getRandomBytesFrom src n
    return (concatBytes xs)

computeNBitInteger :: RandomSource m s => s -> Int -> m Integer
computeNBitInteger src n
    | n .&. 7 == 0
    = computeNByteInteger src (n `shiftR` 3)
    | otherwise
    = do
    x <- computeNByteInteger src ((n `shiftR` 3) + 1)
    return (x .&. (bit n - 1))

computeNByteIntegerWhere :: (RandomSource m s) =>
                            s -> Int -> (Integer -> Bool) -> m Integer
computeNByteIntegerWhere src n p = do
    initialBytes <- getRandomBytesFrom src n
    let mask = 256 ^ n - 1
    
    let compute x
            | p x       = return x
            | otherwise = do
                [newByte] <- getRandomBytesFrom src 1
                compute (mask .&. (x `shiftL` 8) .|. toInteger newByte)
        
    
    compute (concatBytes initialBytes)

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

computeIntegralUniformDist src (Uniform a b) = do
    let m = toInteger b - toInteger a
    
    let bytes = bytesNeeded m
        maxXpossible = (powersOf256 !! bytes) - 1
    
    x <- computeNByteIntegerWhere src bytes (\x -> maxXpossible - x > maxXpossible `mod` m)
    return (a + fromInteger (x `mod` m))

instance Distribution Uniform Float  where sampleFrom = computeRealFloatUniformDist
instance Distribution Uniform Double where sampleFrom = computeRealFloatUniformDist

{-# SPECIALIZE computeRealFloatUniformDist :: RandomSource m s => s -> Uniform Float  -> m Float  #-}
{-# SPECIALIZE computeRealFloatUniformDist :: RandomSource m s => s -> Uniform Double -> m Double #-}

-- (0,1]
computeRealFloatUniformDist src (Uniform 0 f@1) = do
    let bitsNeeded  = floatDigits f
        (_, e) = decodeFloat (1 `asTypeOf` f)
    
    x <- computeNBitInteger src bitsNeeded
    if x == 0
        then return 1
        else return (encodeFloat x (e-1))
    
computeRealFloatUniformDist src (Uniform a b) = do
    let m = b - a
    x <- computeRealFloatUniformDist src (Uniform 0 1)
    return (a + m * x)

bytesNeeded x = case findIndex (> x) powersOf256 of
    Just x -> x
powersOf256 = iterate (256 *) 1

concatBytes :: (Bits a, Num a) => [Word8] -> a
concatBytes = concatBits fromIntegral

concatBits :: (Bits a, Bits b, Num b) => (a -> b) -> [a] -> b
concatBits f [] = 0
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)

