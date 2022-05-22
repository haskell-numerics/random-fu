{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances,
    BangPatterns
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Binomial where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform

import Data.Int
import Data.Word

import Numeric.SpecFunctions ( stirlingError )
import Numeric.SpecFunctions.Extra ( bd0 )
import Numeric ( log1p )

    -- algorithm from Knuth's TAOCP, 3rd ed., p 136
    -- specific choice of cutoff size taken from gsl source
    -- note that although it's fast enough for large (eg, 2^10000)
    -- @Integer@s, it's not accurate enough when using @Double@ as
    -- the @b@ parameter.
integralBinomial :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b, Functor m) => a -> b -> RVarT m a
integralBinomial = bin 0
    where
        bin :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution StdUniform b, Functor m) => a -> a -> b -> RVarT m a
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
                    count !k' 0         = return k'
                    count !k' n | n > 0 = do
                        x <- stdUniformT
                        count (if x < p then k' + 1 else k') (n-1)
                    count _ _ = error "integralBinomial: negative number of trials specified"

integralBinomialCDF :: (Integral a, Real b) => a -> b -> a -> Double
integralBinomialCDF t p x = sum $ map (integralBinomialPDF t p) $ [0 .. x]

-- | The probability of getting exactly k successes in n trials is
-- given by the probability mass function:
--
-- \[
-- f(k;n,p) = \Pr(X = k) = \binom n k  p^k(1-p)^{n-k}
-- \]
--
-- Note that in `integralBinomialPDF` the parameters of the mass
-- function are given first and the range of the random variable
-- distributed according to the binomial distribution is given
-- last. That is, \(f(2;4,0.5)\) is calculated by @integralBinomialPDF 4 0.5 2@.

integralBinomialPDF :: (Integral a, Real b) => a -> b -> a -> Double
integralBinomialPDF t p x =
  exp $ integralBinomialLogPdf t p x

-- | We use the method given in \"Fast and accurate computation of
-- binomial probabilities, Loader, C\",
-- <http://octave.1599824.n4.nabble.com/attachment/3829107/0/loader2000Fast.pdf>
integralBinomialLogPdf :: (Integral a, Real b) => a -> b -> a -> Double
integralBinomialLogPdf nI pR xI
  | p == 0.0 && xI == 0   = 1.0
  | p == 0.0              = 0.0
  | p == 1.0 && xI == nI  = 1.0
  | p == 1.0              = 0.0
  |             xI == 0   = n * log (1-p)
  |             xI == nI  = n * log p
  | otherwise = lc - 0.5 * lf
  where
    n = fromIntegral nI
    x = fromIntegral xI
    p = realToFrac pR
    lc = stirlingError n -
         stirlingError x -
         stirlingError (n - x) -
         bd0 x (n * p) -
         bd0 (n - x) (n * (1 - p))
    lf = log (2 * pi) + log x + log1p (- x / n)

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

floatingBinomialPDF :: (PDF (Binomial b) Integer, RealFrac a) => a -> b -> a -> Double
floatingBinomialPDF t p x = pdf (Binomial (truncate t :: Integer) p) (floor x)

floatingBinomialLogPDF :: (PDF (Binomial b) Integer, RealFrac a) => a -> b -> a -> Double
floatingBinomialLogPDF t p x = logPdf (Binomial (truncate t :: Integer) p) (floor x)

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

{-# SPECIALIZE binomialT :: Functor m => Int     -> Float  -> RVarT m Int #-}
{-# SPECIALIZE binomialT :: Functor m => Int     -> Double -> RVarT m Int #-}
{-# SPECIALIZE binomialT :: Functor m => Integer -> Float  -> RVarT m Integer #-}
{-# SPECIALIZE binomialT :: Functor m => Integer -> Double -> RVarT m Integer #-}
{-# SPECIALIZE binomialT :: Functor m => Float   -> Float  -> RVarT m Float  #-}
{-# SPECIALIZE binomialT :: Functor m => Float   -> Double -> RVarT m Float  #-}
{-# SPECIALIZE binomialT :: Functor m => Double  -> Float  -> RVarT m Double #-}
{-# SPECIALIZE binomialT :: Functor m => Double  -> Double -> RVarT m Double #-}
binomialT :: (Distribution (Binomial b) a, Functor m) => a -> b -> RVarT m a
binomialT t p = rvarT (Binomial t p)

data Binomial b a = Binomial a b

instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Integer where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Integer)                         => CDF (Binomial b) Integer where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Integer)                         => PDF (Binomial b) Integer where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Int where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Int)                             => CDF (Binomial b) Int where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Int)                             => PDF (Binomial b) Int where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Int8 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Int8)                            => CDF (Binomial b) Int8 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Int8)                            => PDF (Binomial b) Int8 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Int16 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Int16)                           => CDF (Binomial b) Int16 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Int16)                           => PDF (Binomial b) Int16 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Int32 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Int32)                           => CDF (Binomial b) Int32 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Int32)                           => PDF (Binomial b) Int32 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Int64 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Int64)                           => CDF (Binomial b) Int64 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Int64)                           => PDF (Binomial b) Int64 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Word where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Word)                            => CDF (Binomial b) Word where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Word)                            => PDF (Binomial b) Word where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Word8 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Word8)                           => CDF (Binomial b) Word8 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Word8)                           => PDF (Binomial b) Word8 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Word16 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Word16)                          => CDF (Binomial b) Word16 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Word16)                          => PDF (Binomial b) Word16 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Word32 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Word32)                          => CDF (Binomial b) Word32 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Word32)                          => PDF (Binomial b) Word32 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p
instance (Floating b, Ord b, Distribution Beta b, Distribution StdUniform b) => Distribution (Binomial b) Word64 where
    rvarT  (Binomial t p) = integralBinomial t p
instance (Real b, Distribution (Binomial b) Word64)                          => CDF (Binomial b) Word64 where
    cdf    (Binomial t p) = integralBinomialCDF t p
instance (Real b, Distribution (Binomial b) Word64)                          => PDF (Binomial b) Word64 where
    pdf    (Binomial t p) = integralBinomialPDF t p
    logPdf (Binomial t p) = integralBinomialLogPdf t p

instance Distribution (Binomial b) Integer => Distribution (Binomial b) Float where
    rvar   (Binomial t p) = floatingBinomial t p
instance CDF (Binomial b) Integer          => CDF (Binomial b) Float where
    cdf    (Binomial t p) = floatingBinomialCDF t p
instance PDF (Binomial b) Integer          => PDF (Binomial b) Float where
    pdf    (Binomial t p) = floatingBinomialPDF t p
    logPdf (Binomial t p) = floatingBinomialLogPDF t p
instance Distribution (Binomial b) Integer => Distribution (Binomial b) Double where
    rvar   (Binomial t p) = floatingBinomial t p
instance CDF (Binomial b) Integer          => CDF (Binomial b) Double where
    cdf    (Binomial t p) = floatingBinomialCDF t p
instance PDF (Binomial b) Integer          => PDF (Binomial b) Double where
    pdf    (Binomial t p) = floatingBinomialPDF t p
    logPdf (Binomial t p) = floatingBinomialLogPDF t p
