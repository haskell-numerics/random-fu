{-# LANGUAGE CPP #-}
module Data.Random.Internal.Fixed where

import Data.Fixed
import Unsafe.Coerce

#ifdef old_Fixed
-- So much for backward compatibility through base (>=5) ...

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

#else

resolutionOf :: HasResolution r => f r -> Integer
resolutionOf = resolution

resolutionOf2 :: HasResolution r => f (g r) -> Integer
resolutionOf2 x = resolution (res x)
    where
        res :: HasResolution r => f (g r) -> g r
        res = undefined

#endif

-- |The 'Fixed' type doesn't expose its constructors, but I need a way to
-- convert them to and from their raw representation in order to sample
-- them.  As long as 'Fixed' is a newtype wrapping 'Integer', 'mkFixed' and
-- 'unMkFixed' as defined here will work.  Both are implemented using
-- 'unsafeCoerce'.
mkFixed :: Integer -> Fixed r
mkFixed = unsafeCoerce

unMkFixed :: Fixed r -> Integer
unMkFixed = unsafeCoerce
