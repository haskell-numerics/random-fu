{-
 -      ``Data/Random/Distribution/Binomial''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances, TemplateHaskell
  #-}

module Data.Random.Distribution.Binomial where

import Data.Random.Internal.TH

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform

import Data.Int
import Data.Word
import Control.Monad

    -- algorithm from Knuth's TAOCP, 3rd ed., p 136
    -- specific choice of cutoff size taken from gsl source
    -- note that although it's fast enough for large (eg, 2^10000) 
    -- @Integer@s, it's not accurate enough when using @Double@ as
    -- the @b@ parameter.
integralBinomial :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => a -> b -> RVar a
integralBinomial t p = bin 0 t p
    where
        -- GHC likes to discharge Beta to the Beta instance's context, which
        -- @integralBinomial@'s context doesn't (directly) satisfy.
        -- Seems like GHC could do better.  GHC could discharge it in @integralBinomial@'s
        -- context when attempting to satisfy bin, since the Beta instance covers all @b@,
        -- whereupon it would find that the contexts do, in fact, match.
        -- Of course, it's a pretty obscure case, so maybe it's better to just 
        -- leave it to the coder who's doing weird stuff to tell the compiler what
        -- he/she wants.
        -- Anyway, this type signature makes GHC happy.
        bin :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => a -> a -> b -> RVar a
        bin k t p
            | t > 10    = do
                let a = 1 + t `div` 2
                    b = 1 + t - a
        
                x <- beta (fromIntegral a) (fromIntegral b)
                if x >= p
                    then bin  k      (a - 1) (p / x)
                    else bin (k + a) (b - 1) ((p - x) / (1 - x))
        
            | otherwise = count k t
                where
                    count k  0    = return k
                    count k (n+1) = do
                        x <- stdUniform
                        (count $! (if x < p then k + 1 else k)) n

-- TODO: improve performance
integralBinomialCDF :: (Integral a, Real b) => a -> b -> a -> Double
integralBinomialCDF n p x = sum
    [ fromIntegral (n `c` i) * p' ^^ i * (1-p') ^^ (n-i)
    | i <- [0 .. x]
    ]
    
    where 
        p' = realToFrac p
        n `c` k = product [n-k+1..n] `div` product [1..k]

-- would it be valid to repeat the above computation using fractional @t@?
-- obviously something different would have to be done with @count@ as well...
floatingBinomial :: (RealFrac a, Distribution (Binomial b) Integer) => a -> b -> RVar a
floatingBinomial t p = fmap fromInteger (rvar (Binomial (truncate t) p))

floatingBinomialCDF :: (CDF (Binomial b) Integer, RealFrac a) => a -> b -> a -> Double
floatingBinomialCDF t p x = cdf (Binomial (truncate t :: Integer) p) (floor x)

binomial :: Distribution (Binomial b) a => a -> b -> RVar a
binomial t p = rvar (Binomial t p)

data Binomial b a = Binomial a b

$( replicateInstances ''Int integralTypes [d|
        instance ( Floating b, Ord b
                 , Distribution Beta b
                 , Distribution StdUniform b
                 ) => Distribution (Binomial b) Int
            where rvar (Binomial t p) = integralBinomial t p
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
