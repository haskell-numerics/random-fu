{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, ForeignFunctionInterface, BangPatterns, 
    RankNTypes
  #-}
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

import Data.Random.Internal.Words
import Data.Bits

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Ziggurat
import Data.Random.RVar

import Data.Vector.Generic (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Data.Number.Erf

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
normalTail :: (Distribution StdUniform a, Floating a, Ord a) =>
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
  b -> (forall m. RVarT m (Int, a)) -> Ziggurat v a
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
realFloatStdNormal :: (RealFloat a, Erf a, Distribution Uniform a) => RVarT m a
realFloatStdNormal = runZiggurat (normalZ p getIU `asTypeOf` (undefined :: Ziggurat V.Vector a))
    where 
        p :: Int
        p = 6
        
        getIU :: (Num a, Distribution Uniform a) => RVarT m (Int, a)
        getIU = do
            i <- getRandomPrim PrimWord8
            u <- uniformT (-1) 1
            return (fromIntegral i .&. (2^p-1), u)

-- |A random variable sampling from the standard normal distribution
-- over the 'Double' type.
doubleStdNormal :: RVarT m Double
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
        getIU :: RVarT m (Int, Double)
        getIU = do
            !w <- getRandomPrim PrimWord64
            let (u,i) = wordToDoubleWithExcess w
            return $! (fromIntegral i .&. (doubleStdNormalC-1), u+u-1)

-- |A random variable sampling from the standard normal distribution
-- over the 'Float' type.
floatStdNormal :: RVarT m Float
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
        getIU :: RVarT m (Int, Float)
        getIU = do
            !w <- getRandomPrim PrimWord32
            let (u,i) = word32ToFloatWithExcess w
            return (fromIntegral i .&. (floatStdNormalC-1), u+u-1)

normalCdf :: (Real a) => a -> a -> a -> Double
normalCdf m s x = normcdf ((realToFrac x - realToFrac m) / realToFrac s)

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

{-# SPECIALIZE stdNormal :: RVar Double #-}
{-# SPECIALIZE stdNormal :: RVar Float #-}
-- |'stdNormal' is a normal variable with distribution 'StdNormal'.
stdNormal :: Distribution Normal a => RVar a
stdNormal = rvar StdNormal

-- |'stdNormalT' is a normal process with distribution 'StdNormal'.
stdNormalT :: Distribution Normal a => RVarT m a
stdNormalT = rvarT StdNormal

-- |@normal m s@ is a random variable with distribution @'Normal' m s@.
normal :: Distribution Normal a => a -> a -> RVar a
normal m s = rvar (Normal m s)

-- |@normalT m s@ is a random process with distribution @'Normal' m s@.
normalT :: Distribution Normal a => a -> a -> RVarT m a
normalT m s = rvarT (Normal m s)
