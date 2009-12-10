{-# LANGUAGE CPP #-}
module Data.Random.Internal.Fixed where

import Data.Fixed
import Unsafe.Coerce

#ifdef base_4_2
-- So much for backward compatibility through base (>=5) ...

resolutionOf :: HasResolution r => f r -> Integer
resolutionOf = resolution

resolutionOf2 :: HasResolution r => f (g r) -> Integer
resolutionOf2 x = resolution (res x)
    where
        res :: HasResolution r => f (g r) -> g r
        res = undefined

#else

resolutionOf :: HasResolution r => f r -> Integer
resolutionOf x = resolution (res x)
    where
        res :: HasResolution r => f r -> r
        res = undefined

resolutionOf2 :: HasResolution r => f (g r) -> Integer
resolutionOf2 x = resolution (res x)
    where
        res :: HasResolution r => f (g r) -> r
        res = undefined

#endif

mkFixed :: Integer -> Fixed r
mkFixed = unsafeCoerce

unMkFixed :: Fixed r -> Integer
unMkFixed = unsafeCoerce