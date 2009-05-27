{-
 -      ``Data/Random/Internal/TH''
 -}
{-# LANGUAGE
        TemplateHaskell
  #-}

module Data.Random.Internal.TH where

import Data.Generics
import Language.Haskell.TH

import Data.Word
import Data.Int

integralTypes = 
    [ ''Int,   ''Integer
    , ''Int8,  ''Int16,  ''Int32,  ''Int64
    , ''Word8, ''Word16, ''Word32, ''Word64
    ]

realFloatTypes =
    [ ''Float, ''Double ]

replaceName :: Name -> Name -> Name -> Name
replaceName x y z
    | x == z    = y
    | otherwise = z

replicateInstances standin types decls = do
    decls <- decls
    sequence
        [ everywhereM (mkM (return . replaceName standin t)) dec
        | t <- types
        , dec <- decls
        ]

