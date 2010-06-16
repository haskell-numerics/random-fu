{-
 -      ``Data/Random/Distribution/Binomial''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances, TemplateHaskell,
    BangPatterns
  #-}

module Data.Random.Distribution.Binomial where

import Data.Random.Internal.TH

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform

    -- algorithm from Knuth's TAOCP, 3rd ed., p 136
    -- specific choice of cutoff size taken from gsl source
    -- note that although it's fast enough for large (eg, 2^10000) 
    -- @Integer@s, it's not accurate enough when using @Double@ as
    -- the @b@ parameter.
integralBinomial :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => a -> b -> RVarT m a
integralBinomial = bin 0
    where
        bin :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => a -> a -> b -> RVarT m a
        bin !k !t !p
            | t > 10    = do
                let a = 1 + t `div` 2
                    b = 1 + t - a
        
                x <- betaT (fromIntegral a) (fromIntegral b)
                if x >= p
                    then bin  k      (a - 1) (p / x)
                    else bin (k + a) (b - 1) ((p - x) / (1 - x))
        
            | otherwise = count k t
                where
                    count !k'  0    = return k'
                    count !k' (n+1) = do
                        x <- stdUniformT
                        count (if x < p then k' + 1 else k') n
                    count _ _ = error "integralBinomial: negative number of trials specified"

-- TODO: improve performance
integralBinomialCDF :: (Integral a, Real b) => a -> b -> a -> Double
integralBinomialCDF t p x = sum
    [ fromInteger (toInteger t `c` toInteger i) * p' ^^ i * (1-p') ^^ (t-i)
    | i <- [0 .. x]
    ]
    
    where 
        p' = realToFrac p
        n `c` k = product [n-k+1..n] `div` product [1..k]

-- would it be valid to repeat the above computation using fractional @t@?
-- obviously something different would have to be done with @count@ as well...
{-# SPECIALIZE floatingBinomial :: Float  -> Float  -> RVar Float  #-}
{-# SPECIALIZE floatingBinomial :: Float  -> Double -> RVar Float  #-}
{-# SPECIALIZE floatingBinomial :: Double -> Float  -> RVar Double #-}
{-# SPECIALIZE floatingBinomial :: Double -> Double -> RVar Double #-}
floatingBinomial :: (RealFrac a, Distribution (Binomial b) Integer) => a -> b -> RVar a
floatingBinomial t p = fmap fromInteger (rvar (Binomial (truncate t) p))

floatingBinomialCDF :: (CDF (Binomial b) Integer, RealFrac a) => a -> b -> a -> Double
floatingBinomialCDF t p x = cdf (Binomial (truncate t :: Integer) p) (floor x)

{-# SPECIALIZE binomial :: Int     -> Float  -> RVar Int #-}
{-# SPECIALIZE binomial :: Int     -> Double -> RVar Int #-}
{-# SPECIALIZE binomial :: Integer -> Float  -> RVar Integer #-}
{-# SPECIALIZE binomial :: Integer -> Double -> RVar Integer #-}
{-# SPECIALIZE binomial :: Float   -> Float  -> RVar Float  #-}
{-# SPECIALIZE binomial :: Float   -> Double -> RVar Float  #-}
{-# SPECIALIZE binomial :: Double  -> Float  -> RVar Double #-}
{-# SPECIALIZE binomial :: Double  -> Double -> RVar Double #-}
binomial :: Distribution (Binomial b) a => a -> b -> RVar a
binomial t p = rvar (Binomial t p)

{-# SPECIALIZE binomialT :: Int     -> Float  -> RVarT m Int #-}
{-# SPECIALIZE binomialT :: Int     -> Double -> RVarT m Int #-}
{-# SPECIALIZE binomialT :: Integer -> Float  -> RVarT m Integer #-}
{-# SPECIALIZE binomialT :: Integer -> Double -> RVarT m Integer #-}
{-# SPECIALIZE binomialT :: Float   -> Float  -> RVarT m Float  #-}
{-# SPECIALIZE binomialT :: Float   -> Double -> RVarT m Float  #-}
{-# SPECIALIZE binomialT :: Double  -> Float  -> RVarT m Double #-}
{-# SPECIALIZE binomialT :: Double  -> Double -> RVarT m Double #-}
binomialT :: Distribution (Binomial b) a => a -> b -> RVarT m a
binomialT t p = rvarT (Binomial t p)

data Binomial b a = Binomial a b

$( replicateInstances ''Int integralTypes [d|
        instance ( Floating b, Ord b
                 , Distribution Beta b
                 , Distribution StdUniform b
                 ) => Distribution (Binomial b) Int
            where 
                rvarT (Binomial t p) = integralBinomial t p
        instance ( Real b , Distribution (Binomial b) Int
                 ) => CDF (Binomial b) Int
            where cdf  (Binomial t p) = integralBinomialCDF t p
    |])

$( replicateInstances ''Float realFloatTypes [d|
        instance Distribution (Binomial b) Integer 
              => Distribution (Binomial b) Float
              where rvar (Binomial t p) = floatingBinomial t p
        instance CDF (Binomial b) Integer
              => CDF (Binomial b) Float
              where cdf  (Binomial t p) = floatingBinomialCDF t p
    |])
