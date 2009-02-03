{-
 -      ``Data/Random/Distribution/Uniform''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleContexts
  #-}

module Data.Random.Distribution.Uniform
    ( Uniform(..)
	, uniform
	
    , StdUniform(..)
    ) where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Uniform.Internal
import Data.Random.RVar

import Data.Word
import Data.Int
import Data.Bits
import Data.List

-- quick & dirty test:
-- let x = sampleFrom DevRandom (Uniform (-100) (100) :: Uniform Int8) :: IO Int8 in mapM_ (\x -> putStrLn (replicate (x `div` 10) '*')) . map length . group . sort =<< replicateM 100000 x

uniform :: Distribution Uniform a => a -> a -> RVar a
uniform a b = sample (Uniform a b)

instance Distribution Uniform Int8      where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int16     where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int32     where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int64     where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Int       where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Integer   where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word8     where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word16    where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word32    where sampleFrom = computeIntegralUniformDist
instance Distribution Uniform Word64    where sampleFrom = computeIntegralUniformDist

instance Distribution StdUniform Int8   where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Int16  where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Int32  where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Int64  where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Int    where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Word8  where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Word16 where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Word32 where sampleFrom = computeBoundedStdUniformDist
instance Distribution StdUniform Word64 where sampleFrom = computeBoundedStdUniformDist

instance Distribution Uniform Float     where sampleFrom = computeRealFloatUniformDist
instance Distribution Uniform Double    where sampleFrom = computeRealFloatUniformDist

instance Distribution StdUniform Float  where sampleFrom = computeFractionalStdUniformDist
instance Distribution StdUniform Double where sampleFrom = computeFractionalStdUniformDist

instance Distribution Uniform Bool      where sampleFrom = computeEnumUniformDist
instance Distribution Uniform Char      where sampleFrom = computeEnumUniformDist
instance Distribution Uniform Ordering  where sampleFrom = computeEnumUniformDist

instance Distribution Uniform ()        where sampleFrom _ _ = return ()

-- instance (Distribution Uniform a, Distribution Uniform b) => Distribution Uniform (a,b) where
--     sampleFrom s (Uniform (a1,b1) (a2,b2)) = do
--         a <- sampleFrom s (Uniform a1 a2)
--         b <- sampleFrom s (Uniform b1 b2)
--         return (a,b)

