{-
 -      ``Data/Random/Distribution/Binomial''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    UndecidableInstances
  #-}

module Data.Random.Distribution.Binomial where

import Data.Random.Source
import Data.Random.Distribution
import Data.Random.RVar

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
integralBinomial :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution Uniform b) => a -> b -> RVar a
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
        bin :: (Integral a, Floating b, Ord b, Distribution Beta b, Distribution Uniform b) => a -> a -> b -> RVar a
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
                        x <- uniform 0 1
                        (count $! (if x < p then k + 1 else k)) n

-- would it be valid to repeat the above computation using fractional @t@?
-- obviously something different would have to be done with @count@ as well...
floatingBinomial :: (RealFrac a, Distribution (Binomial b) Integer) => a -> b -> RVar a
floatingBinomial t p = fmap fromInteger (rvar (Binomial (truncate t) p))

binomial :: Distribution (Binomial b) a => a -> b -> RVar a
binomial t p = rvar (Binomial t p)

data Binomial b a = Binomial a b

instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Int        where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Int8       where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Int16      where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Int32      where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Int64      where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Word8      where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Word16     where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Word32     where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Word64     where rvar (Binomial t p) = integralBinomial t p
instance (Ord b, Floating b, Distribution Beta b, Distribution Uniform b) => Distribution (Binomial b) Integer    where rvar (Binomial t p) = integralBinomial t p

instance Distribution (Binomial b) Integer => Distribution (Binomial b) Float  where rvar (Binomial t p) = floatingBinomial t p
instance Distribution (Binomial b) Integer => Distribution (Binomial b) Double where rvar (Binomial t p) = floatingBinomial t p
