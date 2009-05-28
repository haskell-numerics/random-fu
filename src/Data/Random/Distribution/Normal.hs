{-
 -      ``Data/Random/Distribution/Normal''
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, ForeignFunctionInterface
  #-}

module Data.Random.Distribution.Normal
    ( Normal(..)
    , normal
    , stdNormal
    
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

import Control.Monad
import Foreign.Storable

import Data.Number.Erf

normalPair :: (Floating a, Distribution StdUniform a) => RVar (a,a)
normalPair = boxMullerNormalPair

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

-- |Draw from the tail of a normal distribution (the region beyond the provided value), 
-- returning a negative value if the Bool parameter is True.
{-# INLINE normalTail #-}
normalTail :: (Distribution StdUniform a, Floating a, Ord a) =>
              a -> RVar a
normalTail r = go
    where 
        go = do
            u <- stdUniform
            v <- stdUniform
            let x = log u / r
                y = log v
            if x*x + y+y > 0
                then go
                else return (r - x)

-- |Construct a 'Ziggurat' for sampling a normal distribution, given
-- logBase 2 c, and the 'zGetIU' implementation.
normalZ ::
  (RealFloat a, Erf a, Storable a, Distribution Uniform a, Integral b) =>
  b -> RVar (Int, a) -> Ziggurat a
normalZ p = mkZigguratRec True normalF normalFInv normalFInt normalFVol (2^p)

-- | Ziggurat target function
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

realFloatStdNormal :: (RealFloat a, Erf a, Storable a, Distribution Uniform a) => RVar a
realFloatStdNormal = runZiggurat (normalZ p getIU)
    where 
        p = 6
        
        getIU = do
            i <- getRandomByte
            u <- uniform (-1) 1
            return (fromIntegral i .&. (2^p-1), u)

doubleStdNormal :: RVar Double
doubleStdNormal = runZiggurat doubleStdNormalZ

-- doubleStdNormalC must not be over 12 if using wordToDoubleWithExcess
doubleStdNormalC :: Int
doubleStdNormalC = 512
doubleStdNormalR, doubleStdNormalV :: Double
doubleStdNormalR = 3.852046150368388
doubleStdNormalV = 2.4567663515413507e-3

doubleStdNormalZ :: Ziggurat Double
doubleStdNormalZ = mkZiggurat_ True 
        normalF normalFInv 
        doubleStdNormalC doubleStdNormalR doubleStdNormalV 
        getIU
        (normalTail doubleStdNormalR)
    where 
        getIU = do
            w <- getRandomWord
            let (u,i) = wordToDoubleWithExcess w
            return (fromIntegral i .&. (doubleStdNormalC-1), u+u-1)

floatStdNormal :: RVar Float
floatStdNormal = runZiggurat floatStdNormalZ

floatStdNormalC :: Int
floatStdNormalC = 512
floatStdNormalR, floatStdNormalV :: Float
floatStdNormalR = 3.852046150368388
floatStdNormalV = 2.4567663515413507e-3

floatStdNormalZ :: Ziggurat Float
floatStdNormalZ = mkZiggurat_ True 
        normalF normalFInv 
        floatStdNormalC floatStdNormalR floatStdNormalV 
        getIU
        (normalTail floatStdNormalR)
    where
        -- p must not be over 41 if using wordToFloatWithExcess
        p = 6
        
        getIU = do
            w <- getRandomWord
            let (u,i) = wordToFloatWithExcess w
            return (fromIntegral i .&. (floatStdNormalC-1), u+u-1)

normalPdf :: Real a => a -> a -> a -> Double
normalPdf m s x = recip (realToFrac s * sqrt (2*pi)) * exp (-0.5 * (realToFrac x - realToFrac m)^2 / (realToFrac s)^2)

normalCdf :: (Real a, Erf a) => a -> a -> a -> Double
normalCdf m s x = realToFrac (normcdf ((x-m) / s))

data Normal a
    = StdNormal
    | Normal a a -- mean, sd

instance Distribution Normal Double where
    {-# SPECIALIZE instance Distribution Normal Double #-}
    rvar StdNormal = doubleStdNormal
    rvar (Normal m s) = do
        x <- doubleStdNormal
        return (x * s + m)

instance Distribution Normal Float where
    {-# SPECIALIZE instance Distribution Normal Float #-}
    rvar StdNormal = floatStdNormal
    rvar (Normal m s) = do
        x <- floatStdNormal
        return (x * s + m)

instance (Real a, Erf a, Distribution Normal a) => CDF Normal a where
    cdf StdNormal    = normalCdf 0 1
    cdf (Normal m s) = normalCdf m s

{-# SPECIALIZE stdNormal :: RVar Double #-}
{-# SPECIALIZE stdNormal :: RVar Float #-}
stdNormal :: Distribution Normal a => RVar a
stdNormal = rvar StdNormal

normal :: Distribution Normal a => a -> a -> RVar a
normal m s = rvar (Normal m s)