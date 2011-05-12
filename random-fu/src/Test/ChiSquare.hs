{-# LANGUAGE ParallelListComp #-}
module Test.ChiSquare where

import Data.Random
import Data.Random.Distribution.ChiSquare
import qualified Data.Map as M
import Data.Maybe

summarizeObservations events observations = M.unionWith (+) zeros freqs
    where 
        zeros = M.fromList         [(evt, 0) | evt <- events]
        freqs = M.fromListWith (+) [(evt, 1) | evt <- observations]

chiSquareStatistic pmf n freqs = sum
    [ square (realToFrac observed - expected) / expected
    | (event, observed) <- M.toList freqs
    , let expected = pmf event * realToFrac n
    ] 
    where
        square x = x*x

chiSquareTest pmf freqs = cdf (ChiSquare (M.size freqs - 1)) chiSq
    where
        chiSq :: Double
        chiSq = chiSquareStatistic pmf n freqs
        n = sum (M.elems freqs)
