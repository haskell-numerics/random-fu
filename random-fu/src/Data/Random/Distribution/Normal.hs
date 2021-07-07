{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, ForeignFunctionInterface, BangPatterns,
    RankNTypes
  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Random.Distribution.Normal
    ( Normal(..)
    , normal, normalT
    , stdNormal, stdNormalT

    , doubleStdNormal
    , floatStdNormal
    , realFloatStdNormal

    , normalTail

    , normalPair
    , boxMullerNormalPair
    , knuthPolarNormalPair
    ) where

import Data.Bits

import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Ziggurat
import Data.Random.RVar
import Data.Word

import Data.Vector.Generic (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Data.Number.Erf

import qualified System.Random.Stateful as Random

-- |A random variable that produces a pair of independent
-- normally-distributed values.
normalPair :: (Floating a, Distribution StdUniform a) => RVar (a,a)
normalPair = boxMullerNormalPair

-- |A random variable that produces a pair of independent
-- normally-distributed values, computed using the Box-Muller method.
-- This algorithm is slightly slower than Knuth's method but using a
-- constant amount of entropy (Knuth's method is a rejection method).
-- It is also slightly more general (Knuth's method require an 'Ord'
-- instance).
{-# INLINE boxMullerNormalPair #-}
boxMullerNormalPair :: (Floating a, Distribution StdUniform a) => RVar (a,a)
boxMullerNormalPair = do
    u <- stdUniform
    t <- stdUniform
    let r = sqrt (-2 * log u)
        theta = (2 * pi) * t

        x = r * cos theta
        y = r * sin theta
    return (x,y)

-- |A random variable that produces a pair of independent
-- normally-distributed values, computed using Knuth's polar method.
-- Slightly faster than 'boxMullerNormalPair' when it accepts on the
-- first try, but does not always do so.
{-# INLINE knuthPolarNormalPair #-}
knuthPolarNormalPair :: (Floating a, Ord a, Distribution Uniform a) => RVar (a,a)
knuthPolarNormalPair = do
    v1 <- uniform (-1) 1
    v2 <- uniform (-1) 1

    let s = v1*v1 + v2*v2
    if s >= 1
        then knuthPolarNormalPair
        else return $ if s == 0
            then (0,0)
            else let scale = sqrt (-2 * log s / s)
                  in (v1 * scale, v2 * scale)

-- |Draw from the tail of a normal distribution (the region beyond the provided value)
{-# INLINE normalTail #-}
normalTail :: (Distribution StdUniform a, Floating a, Ord a, Functor m) =>
              a -> RVarT m a
normalTail r = go
    where
        go = do
            !u <- stdUniformT
            let !x = log u / r
            !v <- stdUniformT
            let !y = log v
            if x*x + y+y > 0
                then go
                else return (r - x)

-- |Construct a 'Ziggurat' for sampling a normal distribution, given
-- @logBase 2 c@ and the 'zGetIU' implementation.
normalZ ::
  (RealFloat a, Erf a, Vector v a, Distribution Uniform a, Integral b) =>
  b -> (forall m. Functor m => RVarT m (Int, a)) -> Ziggurat v a
normalZ p = mkZigguratRec True normalF normalFInv normalFInt normalFVol (2^p)

-- | Ziggurat target function (upper half of a non-normalized gaussian PDF)
normalF :: (Floating a, Ord a) => a -> a
normalF x
    | x <= 0    = 1
    | otherwise = exp ((-0.5) * x*x)
-- | inverse of 'normalF'
normalFInv :: Floating a => a -> a
normalFInv y  = sqrt ((-2) * log y)
-- | integral of 'normalF'
normalFInt :: (Floating a, Erf a, Ord a) => a -> a
normalFInt x
    | x <= 0    = 0
    | otherwise = normalFVol * erf (x * sqrt 0.5)
-- | volume of 'normalF'
normalFVol :: Floating a => a
normalFVol = sqrt (0.5 * pi)

-- |A random variable sampling from the standard normal distribution
-- over any 'RealFloat' type (subject to the rest of the constraints -
-- it builds and uses a 'Ziggurat' internally, which requires the 'Erf'
-- class).
--
-- Because it computes a 'Ziggurat', it is very expensive to use for
-- just one evaluation, or even for multiple evaluations if not used and
-- reused monomorphically (to enable the ziggurat table to be let-floated
-- out).  If you don't know whether your use case fits this description
-- then you're probably better off using a different algorithm, such as
-- 'boxMullerNormalPair' or 'knuthPolarNormalPair'.  And of course if
-- you don't need the full generality of this definition then you're much
-- better off using 'doubleStdNormal' or 'floatStdNormal'.
--
-- As far as I know, this should be safe to use in any monomorphic
-- @Distribution Normal@ instance declaration.
realFloatStdNormal :: (RealFloat a, Erf a, Distribution Uniform a, Functor m) => RVarT m a
realFloatStdNormal = runZiggurat (normalZ p getIU `asTypeOf` (undefined :: Ziggurat V.Vector a))
    where
        p :: Int
        p = 6

        getIU :: (Num a, Distribution Uniform a, Functor m) => RVarT m (Int, a)
        getIU = do
            i <- Random.uniformWord8 RGen
            u <- uniformT (-1) 1
            return (fromIntegral i .&. (2^p-1), u)

-- |A random variable sampling from the standard normal distribution
-- over the 'Double' type.
doubleStdNormal :: Functor m => RVarT m Double
doubleStdNormal = runZiggurat doubleStdNormalZ

-- doubleStdNormalC must not be over 2^12 if using wordToDoubleWithExcess
doubleStdNormalC :: Int
doubleStdNormalC = 512
doubleStdNormalR, doubleStdNormalV :: Double
doubleStdNormalR = 3.852046150368388
doubleStdNormalV = 2.4567663515413507e-3

{-# NOINLINE doubleStdNormalZ #-}
doubleStdNormalZ :: Ziggurat UV.Vector Double
doubleStdNormalZ = mkZiggurat_ True
        normalF normalFInv
        doubleStdNormalC doubleStdNormalR doubleStdNormalV
        getIU
        (normalTail doubleStdNormalR)
    where
        getIU :: Functor m => RVarT m (Int, Double)
        getIU = do
            !w <- Random.uniformWord64 RGen
            let (u,i) = wordToDoubleWithExcess w
            return $! (fromIntegral i .&. (doubleStdNormalC-1), u+u-1)

-- NOTE: inlined from random-source
{-# INLINE wordToDouble #-}
-- |Pack the low 52 bits from a 'Word64' into a 'Double' in the range [0,1).
-- Used to convert a 'stdUniform' 'Word64' to a 'stdUniform' 'Double'.
wordToDouble :: Word64 -> Double
wordToDouble x = (encodeFloat $! toInteger (x .&. 0x000fffffffffffff {- 2^52-1 -})) $ (-52)

{-# INLINE wordToDoubleWithExcess #-}
-- |Same as wordToDouble, but also return the unused bits (as the 12
-- least significant bits of a 'Word64')
wordToDoubleWithExcess :: Word64 -> (Double, Word64)
wordToDoubleWithExcess x = (wordToDouble x, x `shiftR` 52)


-- |A random variable sampling from the standard normal distribution
-- over the 'Float' type.
floatStdNormal :: Functor m => RVarT m Float
floatStdNormal = runZiggurat floatStdNormalZ

-- floatStdNormalC must not be over 2^9 if using word32ToFloatWithExcess
floatStdNormalC :: Int
floatStdNormalC = 512
floatStdNormalR, floatStdNormalV :: Float
floatStdNormalR = 3.852046150368388
floatStdNormalV = 2.4567663515413507e-3

{-# NOINLINE floatStdNormalZ #-}
floatStdNormalZ :: Ziggurat UV.Vector Float
floatStdNormalZ = mkZiggurat_ True
        normalF normalFInv
        floatStdNormalC floatStdNormalR floatStdNormalV
        getIU
        (normalTail floatStdNormalR)
    where
        getIU :: Functor m => RVarT m (Int, Float)
        getIU = do
            !w <- Random.uniformWord32 RGen
            let (u,i) = word32ToFloatWithExcess w
            return (fromIntegral i .&. (floatStdNormalC-1), u+u-1)

-- NOTE: inlined from random-source
{-# INLINE word32ToFloat #-}
-- |Pack the low 23 bits from a 'Word32' into a 'Float' in the range [0,1).
-- Used to convert a 'stdUniform' 'Word32' to a 'stdUniform' 'Double'.
word32ToFloat :: Word32 -> Float
word32ToFloat x = (encodeFloat $! toInteger (x .&. 0x007fffff {- 2^23-1 -} )) $ (-23)

{-# INLINE word32ToFloatWithExcess #-}
-- |Same as word32ToFloat, but also return the unused bits (as the 9
-- least significant bits of a 'Word32')
word32ToFloatWithExcess :: Word32 -> (Float, Word32)
word32ToFloatWithExcess x = (word32ToFloat x, x `shiftR` 23)


normalCdf :: (Real a) => a -> a -> a -> Double
normalCdf m s x = normcdf ((realToFrac x - realToFrac m) / realToFrac s)

normalPdf :: (Real a, Floating b) => a -> a -> a -> b
normalPdf mu sigma x =
  (recip (sqrt (2 * pi * sigma2))) * (exp ((-((realToFrac x) - (realToFrac mu))^2) / (2 * sigma2)))
  where
    sigma2 = realToFrac sigma^2

normalLogPdf :: (Real a, Floating b) => a -> a -> a -> b
normalLogPdf mu sigma x =
  log (recip (sqrt (2 * pi * sigma2))) +
  ((-((realToFrac x) - (realToFrac mu))^2) / (2 * sigma2))
  where
    sigma2 = realToFrac sigma^2

-- |A specification of a normal distribution over the type 'a'.
data Normal a
    -- |The \"standard\" normal distribution - mean 0, stddev 1
    = StdNormal
    -- |@Normal m s@ is a normal distribution with mean @m@ and stddev @sd@.
    | Normal a a -- mean, sd

instance Distribution Normal Double where
    rvarT StdNormal = doubleStdNormal
    rvarT (Normal m s) = do
        x <- doubleStdNormal
        return (x * s + m)

instance Distribution Normal Float where
    rvarT StdNormal = floatStdNormal
    rvarT (Normal m s) = do
        x <- floatStdNormal
        return (x * s + m)

instance (Real a, Distribution Normal a) => CDF Normal a where
    cdf StdNormal    = normalCdf 0 1
    cdf (Normal m s) = normalCdf m s

instance (Real a, Floating a, Distribution Normal a) => PDF Normal a where
  pdf StdNormal    = normalPdf 0 1
  pdf (Normal m s) = normalPdf m s
  logPdf StdNormal = normalLogPdf 0 1
  logPdf (Normal m s) = normalLogPdf m s

{-# SPECIALIZE stdNormal :: RVar Double #-}
{-# SPECIALIZE stdNormal :: RVar Float #-}
-- |'stdNormal' is a normal variable with distribution 'StdNormal'.
stdNormal :: Distribution Normal a => RVar a
stdNormal = rvar StdNormal

-- |'stdNormalT' is a normal process with distribution 'StdNormal'.
stdNormalT :: (Distribution Normal a, Functor m) => RVarT m a
stdNormalT = rvarT StdNormal

-- |@normal m s@ is a random variable with distribution @'Normal' m s@.
normal :: Distribution Normal a => a -> a -> RVar a
normal m s = rvar (Normal m s)

-- |@normalT m s@ is a random process with distribution @'Normal' m s@.
normalT :: (Distribution Normal a, Functor m) => a -> a -> RVarT m a
normalT m s = rvarT (Normal m s)
