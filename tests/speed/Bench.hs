{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Random

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans (lift)
import Data.List
import qualified Data.Vector as V
import Foreign
import System.IO
import TestSupport

import Criterion.Main

main = do
    src <- getTestSource
    let count = 64000
    
    defaultMain
        [ suite src count "uniform"     (Uniform 10 50)
        , suite src count "stdUniform"  StdUniform
        , suite src count "stdNormal"   StdNormal
        , suite src count "exponential" (Exp  2)
        , suite src count "beta"        (Beta  2 5)
        , suite src count "gamma"       (Gamma 2 5)
        , suite src count "triangular"  (Triangular 2 5 14)
        , suite src count "rayleigh"    (Rayleigh 1.6)
        , suite src count "poisson"     (Poisson 3 :: Poisson Double Double)
        , suite src count "binomial 10" (Binomial 10 (0.5 :: Float))
        
        , bench "dirichlet" $ do
            xs <- sampleFrom src (dirichlet [1..fromIntegral count :: Double])
            foldl1' (+) xs `seq` return () :: IO ()
            
        , bgroup "multinomial" 
            [ bgroup "many p"
                [ bench desc $ do
                    xs <- sampleFrom src (multinomial [1..1e4 :: Double] (n :: Int))
                    foldl1' (+) xs `seq` return () :: IO ()
                | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
                ]
            , bgroup "few p" 
                [bench desc $ do
                    replicateM_ 1000 $ do
                        xs <- sampleFrom src (multinomial [1..10 :: Double] (n :: Int))
                        foldl1' (+) xs `seq` return () :: IO ()
                | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
                ]
            ]
            
        , bench "shuffle" $ do
            xs <- sampleFrom src (shuffle [1..count])
            foldl1' (+) xs `seq` return () :: IO ()
        ]

-- Ideally, these would all be the same speed
suite :: Distribution d Double => Src -> Int -> String -> d Double -> Benchmark
suite src count name var = bgroup name 
    [ bench "sum of samples (implicit rvar)" $ do
        x <- sumM count (sampleFrom src var) :: IO Double
        x `seq` return () :: IO ()

    , bench "sum of samples (explicit rvar)" $ do
        x <- sumM count (sampleFrom src (rvar var)) :: IO Double
        x `seq` return () :: IO ()

    , bench "sample of sum" $ do
        x <- sampleFrom src (sumM count (rvar var)) :: IO Double
        x `seq` return () :: IO ()
    
    , let !bufSiz = count * 8
       in bench "array of samples" $ do
            allocaBytes bufSiz $ \ptr -> do
                sequence_
                    [ do
                        x <- sampleFrom src var
            
                        pokeByteOff ptr offset x
                    | offset <- [0,8..bufSiz - 8]
                    ]
    
    , let !bufSiz = count * 8
       in bench "RVarT IO arrays" $ do
            allocaBytes bufSiz $ \ptr -> flip runRVarT src $ do
                sequence_
                    [ do
                        x <- rvarT var
            
                        lift (pokeByteOff ptr offset x)
                    | offset <- [0,8..bufSiz - 8]
                    ]
    ]
