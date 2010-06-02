{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Data.Random.Distribution.Weibull
    ( mkWeibull, Weibull
    , weibullLambda, weibullK
    ) where

import Data.Random.Distribution
import Data.Random.Distribution.Uniform

data Weibull a = Weibull { weibullLambda :: !a, weibullK :: !a }
    deriving (Eq, Show)
mkWeibull lambda k = Weibull lambda k

instance (Floating a, Distribution StdUniform a) => Distribution Weibull a where
    rvarT (Weibull lambda k) = do
        u <- rvarT StdUniform
        return (lambda * (negate (log u)) ** recip k)

instance (Real a, Distribution Weibull a) => CDF Weibull a where
    cdf (Weibull lambda k) x = 1 - exp (negate ((realToFrac x / realToFrac lambda) ** realToFrac k))