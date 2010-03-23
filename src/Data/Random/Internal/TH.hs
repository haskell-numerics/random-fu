{-
 -      ``Data/Random/Internal/TH''
 -}
{-# LANGUAGE
        TemplateHaskell
  #-}

-- |Template Haskell utility code to replicate instance declarations
-- to cover large numbers of types.  I'm doing that rather than using
-- class contexts because most Distribution instances need to cover
-- multiple classes (such as Enum, Integral and Fractional) and that
-- can't be done easily because of overlap.  
-- 
-- I experimented a bit with a convoluted type-level classification 
-- scheme, but I think this is simpler and easier to understand.  It 
-- makes the haddock docs more cluttered because of the combinatorial 
-- explosion of instances, but overall I think it's just more sane than 
-- anything else I've come up with yet.
module Data.Random.Internal.TH
    ( replicateInstances
    , integralTypes, realFloatTypes
    ) where

import Data.Generics
import Language.Haskell.TH

import Data.Word
import Data.Int

-- |Names of standard 'Integral' types
integralTypes :: [Name]
integralTypes = 
    [ ''Int,   ''Integer
    , ''Int8,  ''Int16,  ''Int32,  ''Int64
    , ''Word8, ''Word16, ''Word32, ''Word64
    ]

-- |Names of standard 'RealFloat' types
realFloatTypes :: [Name]
realFloatTypes =
    [ ''Float, ''Double ]

-- @replaceName x y@ is a function that will
-- replace @x@ with @y@ whenever it sees it.  That is:
--
-- > replaceName x y x  ==>  y
-- > replaceName x y z  ==>  z
--  (@z /= x@)
replaceName :: Name -> Name -> Name -> Name
replaceName x y z
    | x == z    = y
    | otherwise = z

-- | @replicateInstances standin types decls@ will take the template-haskell
-- 'Dec's in @decls@ and substitute every instance of the 'Name' @standin@ with
-- each 'Name' in @types@, producing one copy of the 'Dec's in @decls@ for every
-- 'Name' in @types@.
-- 
-- For example, 'Data.Random.Distribution.Uniform' has the following bit of TH code:
-- 
-- @ $( replicateInstances ''Int integralTypes [d|                                                  @
-- 
-- @       instance Distribution Uniform Int   where rvar (Uniform a b) = integralUniform a b       @
-- 
-- @       instance CDF Uniform Int            where cdf  (Uniform a b) = integralUniformCDF a b    @
-- 
-- @   |])                                                                                          @
-- 
-- This code takes those 2 instance declarations and creates identical ones for
-- every type named in 'integralTypes'.
replicateInstances :: (Monad m, Data t) => Name -> [Name] -> m [t] -> m [t]
replicateInstances standin types getDecls = do
    decls <- getDecls
    sequence
        [ everywhereM (mkM (return . replaceName standin t)) dec
        | t <- types
        , dec <- decls
        ]

