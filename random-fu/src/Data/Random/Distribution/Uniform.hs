{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances,
    UndecidableInstances, EmptyDataDecls,
    TemplateHaskell,
    BangPatterns
  #-}

module Data.Random.Distribution.Uniform
    ( Uniform(..)
    , uniform
    , uniformT

    , StdUniform(..)
    , stdUniform
    , stdUniformT
    , stdUniformPos
    , stdUniformPosT

    , integralUniform
    , realFloatUniform
    , floatUniform
    , doubleUniform
    , fixedUniform
    , enumUniform

    , boundedStdUniform
    , boundedEnumStdUniform
    , realFloatStdUniform
    , fixedStdUniform
    , floatStdUniform
    , doubleStdUniform

    , boundedStdUniformCDF
    , realStdUniformCDF
    , realUniformCDF
    , enumUniformCDF
    ) where


import Data.Random.Internal.Fixed

import Data.Random.Distribution
import Data.Random.RVar

import Data.Fixed
import Data.Word
import Data.Int

import Control.Monad.Loops

import qualified System.Random.Stateful as Random

-- |Compute a random 'Integral' value between the 2 values provided (inclusive).
{-# INLINE integralUniform #-}
integralUniform :: Random.UniformRange a => a -> a -> RVarT m a
integralUniform !x !y = Random.uniformRM (x, y) RGen
  -- Maybe switch to uniformIntegralM (requires exposing from `random` internals):
  -- Random.uniformIntegralM (x, y) RGen

integralUniformCDF :: (Integral a, Fractional b) => a -> a -> a -> b
integralUniformCDF a b x
    | b < a     = integralUniformCDF b a x
    | x < a     = 0
    | x > b     = 1
    | otherwise = (fromIntegral x - fromIntegral a) / (fromIntegral b - fromIntegral a)

-- |Compute a random value for a 'Bounded' type, between 'minBound' and 'maxBound'
-- (inclusive for 'Integral' or 'Enum' types, in ['minBound', 'maxBound') for Fractional types.)
boundedStdUniform :: (Distribution Uniform a, Bounded a) => RVar a
boundedStdUniform = uniform minBound maxBound

boundedStdUniformCDF :: (CDF Uniform a, Bounded a) => a -> Double
boundedStdUniformCDF = cdf (Uniform minBound maxBound)

-- |Compute a random value for a 'Bounded' 'Enum' type, between 'minBound' and
-- 'maxBound' (inclusive)
boundedEnumStdUniform :: (Enum a, Bounded a) => RVarT m a
boundedEnumStdUniform = enumUniform minBound maxBound

boundedEnumStdUniformCDF :: (Enum a, Bounded a, Ord a) => a -> Double
boundedEnumStdUniformCDF = enumUniformCDF minBound maxBound

-- |Compute a uniform random 'Float' value in the range [0,1)
floatStdUniform :: RVarT m Float
floatStdUniform = do
    x <- uniformRangeRVarT (0, 1)
    -- exclude 1. TODO: come up with something smarter
    if x == 1 then floatStdUniform else pure x

-- |Compute a uniform random 'Double' value in the range [0,1)
{-# INLINE doubleStdUniform #-}
doubleStdUniform :: RVarT m Double
doubleStdUniform = do
    x <- uniformRangeRVarT (0, 1)
    -- exclude 1. TODO: come up with something smarter
    if x == 1 then doubleStdUniform else pure x

-- |Compute a uniform random value in the range [0,1) for any 'RealFloat' type
realFloatStdUniform :: RealFloat a => RVarT m a
realFloatStdUniform = do
    let (b, e) = decodeFloat one

    x <- uniformT 0 (b-1)
    if x == 0
        then return (0 `asTypeOf` one)
        else return (encodeFloat x e)

    where one = 1

-- |Compute a uniform random 'Fixed' value in the range [0,1), with any
-- desired precision.
fixedStdUniform :: HasResolution r => RVarT m (Fixed r)
fixedStdUniform = x
    where
        res = resolutionOf2 x
        x = do
            u <- uniformT 0 (res)
            return (mkFixed u)

-- |The CDF of the random variable 'realFloatStdUniform'.
realStdUniformCDF :: Real a => a -> Double
realStdUniformCDF x
    | x <= 0    = 0
    | x >= 1    = 1
    | otherwise = realToFrac x

-- |The PDF of the random variable 'realFloatStdUniform'.
realStdUniformPDF :: Real a => a -> Double
realStdUniformPDF x
    | x <= 0    = 0
    | x >= 1    = 0
    | otherwise = 1

-- |(internal) basic linear interpolation; @lerp x y@ is a linear function whose
-- value is @x@ at 0 and @y@ at 1
lerp :: Num a => a -> a -> a -> a
lerp x y a = (1-a)*x + a*y

-- |@floatUniform a b@ computes a uniform random 'Float' value in the range [a,b)
floatUniform :: Float -> Float -> RVarT m Float
floatUniform 0 1 = floatStdUniform
floatUniform a b = do
    x <- floatStdUniform
    return (lerp a b x)

-- |@doubleUniform a b@ computes a uniform random 'Double' value in the range [a,b)
{-# INLINE doubleUniform #-}
doubleUniform :: Double -> Double -> RVarT m Double
doubleUniform 0 1 = doubleStdUniform
doubleUniform a b = do
    x <- doubleStdUniform
    return (lerp a b x)

-- |@realFloatUniform a b@ computes a uniform random value in the range [a,b) for
-- any 'RealFloat' type
realFloatUniform :: RealFloat a => a -> a -> RVarT m a
realFloatUniform 0 1 = realFloatStdUniform
realFloatUniform a b = do
    x <- realFloatStdUniform
    return (lerp a b x)

-- |@fixedUniform a b@ computes a uniform random 'Fixed' value in the range
-- [a,b), with any desired precision.
fixedUniform :: HasResolution r => Fixed r -> Fixed r -> RVarT m (Fixed r)
fixedUniform a b = do
    u <- integralUniform (unMkFixed a) (unMkFixed b)
    return (mkFixed u)

-- |@realUniformCDF a b@ is the CDF of the random variable @realFloatUniform a b@.
realUniformCDF :: RealFrac a => a -> a -> a -> Double
realUniformCDF a b x
    | b < a     = realUniformCDF b a x
    | x <= a    = 0
    | x >= b    = 1
    | otherwise = realToFrac ((x-a) / (b-a))

-- |@realFloatUniform a b@ computes a uniform random value in the range [a,b) for
-- any 'Enum' type
enumUniform :: Enum a => a -> a -> RVarT m a
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

-- @uniform a b@ is a uniformly distributed random variable in the range
-- [a,b] for 'Integral' or 'Enum' types and in the range [a,b) for 'Fractional'
-- types.  Requires a @Distribution Uniform@ instance for the type.
uniform :: Distribution Uniform a => a -> a -> RVar a
uniform a b = rvar (Uniform a b)

-- @uniformT a b@ is a uniformly distributed random process in the range
-- [a,b] for 'Integral' or 'Enum' types and in the range [a,b) for 'Fractional'
-- types.  Requires a @Distribution Uniform@ instance for the type.
uniformT :: Distribution Uniform a => a -> a -> RVarT m a
uniformT a b = rvarT (Uniform a b)

-- |Get a \"standard\" uniformly distributed variable.
-- For integral types, this means uniformly distributed over the full range
-- of the type (there is no support for 'Integer').  For fractional
-- types, this means uniformly distributed on the interval [0,1).
{-# SPECIALIZE stdUniform :: RVar Double #-}
{-# SPECIALIZE stdUniform :: RVar Float #-}
stdUniform :: (Distribution StdUniform a) => RVar a
stdUniform = rvar StdUniform

-- |Get a \"standard\" uniformly distributed process.
-- For integral types, this means uniformly distributed over the full range
-- of the type (there is no support for 'Integer').  For fractional
-- types, this means uniformly distributed on the interval [0,1).
{-# SPECIALIZE stdUniformT :: RVarT m Double #-}
{-# SPECIALIZE stdUniformT :: RVarT m Float #-}
stdUniformT :: (Distribution StdUniform a) => RVarT m a
stdUniformT = rvarT StdUniform

-- |Like 'stdUniform', but returns only positive or zero values.  Not
-- exported because it is not truly uniform: nonzero values are twice
-- as likely as zero on signed types.
stdUniformNonneg :: (Distribution StdUniform a, Num a, Eq a) => RVarT m a
stdUniformNonneg = fmap abs stdUniformT

-- |Like 'stdUniform' but only returns positive values.
stdUniformPos :: (Distribution StdUniform a, Num a, Eq a) => RVar a
stdUniformPos = stdUniformPosT

-- |Like 'stdUniform' but only returns positive values.
stdUniformPosT :: (Distribution StdUniform a, Num a, Eq a) => RVarT m a
stdUniformPosT = iterateUntil (/= 0) stdUniformNonneg

-- |A definition of a uniform distribution over the type @t@.  See also 'uniform'.
data Uniform t =
    -- |A uniform distribution defined by a lower and upper range bound.
    -- For 'Integral' and 'Enum' types, the range is inclusive.  For 'Fractional'
    -- types the range includes the lower bound but not the upper.
    Uniform !t !t

-- |A name for the \"standard\" uniform distribution over the type @t@,
-- if one exists.  See also 'stdUniform'.
--
-- For 'Integral' and 'Enum' types that are also 'Bounded', this is
-- the uniform distribution over the full range of the type.
-- For un-'Bounded' 'Integral' types this is not defined.
-- For 'Fractional' types this is a random variable in the range [0,1)
-- (that is, 0 to 1 including 0 but not including 1).
data StdUniform t = StdUniform

instance Distribution Uniform Integer where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Integer          where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Int     where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Int              where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Int8    where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Int8             where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Int16   where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Int16            where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Int32   where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Int32            where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Int64   where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Int64            where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Word    where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Word             where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Word8   where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Word8            where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Word16  where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Word16           where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Word32  where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Word32           where cdf   (Uniform a b) = integralUniformCDF a b
instance Distribution Uniform Word64  where rvarT (Uniform a b) = integralUniform a b
instance CDF Uniform Word64           where cdf   (Uniform a b) = integralUniformCDF a b

instance Distribution StdUniform Word8      where rvarT _ = Random.uniformWord8 RGen
instance Distribution StdUniform Word16     where rvarT _ = Random.uniformWord16 RGen
instance Distribution StdUniform Word32     where rvarT _ = Random.uniformWord32 RGen
instance Distribution StdUniform Word64     where rvarT _ = Random.uniformWord64 RGen
instance Distribution StdUniform Word       where rvarT _ = uniformRVarT

instance Distribution StdUniform Int8       where rvarT _ = uniformRVarT
instance Distribution StdUniform Int16      where rvarT _ = uniformRVarT
instance Distribution StdUniform Int32      where rvarT _ = uniformRVarT
instance Distribution StdUniform Int64      where rvarT _ = uniformRVarT

instance Distribution StdUniform Int        where rvarT _ = uniformRVarT


-- Integer has no StdUniform...

instance CDF StdUniform Word8   where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Word16  where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Word32  where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Word64  where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Word    where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Int8    where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Int16   where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Int32   where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Int64   where cdf _ = integralUniformCDF minBound maxBound
instance CDF StdUniform Int     where cdf _ = integralUniformCDF minBound maxBound


instance Distribution Uniform Float         where rvarT (Uniform a b) = floatUniform  a b
instance Distribution Uniform Double        where rvarT (Uniform a b) = doubleUniform a b
instance CDF Uniform Float                  where cdf   (Uniform a b) = realUniformCDF a b
instance CDF Uniform Double                 where cdf   (Uniform a b) = realUniformCDF a b

instance Distribution StdUniform Float      where rvarT _ = floatStdUniform
instance Distribution StdUniform Double     where rvarT _ = uniformRangeRVarT (0, 1)
instance CDF StdUniform Float               where cdf   _ = realStdUniformCDF
instance CDF StdUniform Double              where cdf   _ = realStdUniformCDF
instance PDF StdUniform Float               where pdf   _ = realStdUniformPDF
instance PDF StdUniform Double              where pdf   _ = realStdUniformPDF


instance HasResolution r =>
         Distribution Uniform (Fixed r)     where rvarT (Uniform a b) = fixedUniform  a b
instance HasResolution r =>
         CDF Uniform (Fixed r)              where cdf   (Uniform a b) = realUniformCDF a b
instance HasResolution r =>
         Distribution StdUniform (Fixed r)  where rvarT ~StdUniform = fixedStdUniform
instance HasResolution r =>
         CDF StdUniform (Fixed r)           where cdf   ~StdUniform = realStdUniformCDF

instance Distribution Uniform ()            where rvarT (Uniform _ _) = return ()
instance CDF Uniform ()                     where cdf   (Uniform _ _) = return 1

instance Distribution Uniform Char     where rvarT (Uniform a b) = enumUniform a b
instance CDF Uniform Char              where cdf   (Uniform a b) = enumUniformCDF a b
instance Distribution Uniform Bool     where rvarT (Uniform a b) = enumUniform a b
instance CDF Uniform Bool              where cdf   (Uniform a b) = enumUniformCDF a b
instance Distribution Uniform Ordering where rvarT (Uniform a b) = enumUniform a b
instance CDF Uniform Ordering          where cdf   (Uniform a b) = enumUniformCDF a b

instance Distribution StdUniform ()         where rvarT ~StdUniform = return ()
instance CDF StdUniform ()                  where cdf   ~StdUniform = return 1
instance Distribution StdUniform Bool       where rvarT ~StdUniform = uniformRVarT
instance CDF StdUniform Bool                where cdf   ~StdUniform = boundedEnumStdUniformCDF

instance Distribution StdUniform Char       where rvarT ~StdUniform = boundedEnumStdUniform
instance CDF StdUniform Char                where cdf   ~StdUniform = boundedEnumStdUniformCDF
instance Distribution StdUniform Ordering   where rvarT ~StdUniform = boundedEnumStdUniform
instance CDF StdUniform Ordering            where cdf   ~StdUniform = boundedEnumStdUniformCDF
