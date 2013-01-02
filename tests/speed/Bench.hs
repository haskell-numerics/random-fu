{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Main where

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Dirichlet
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Multinomial
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Rayleigh
import Data.Random.Distribution.Triangular


import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import qualified Control.Monad.Random as CMR
import Control.Monad.Trans (lift)
import Data.List
import qualified Data.Vector as V
import Foreign
import System.IO
import System.Random
import qualified System.Random.MWC as MWC
import TestSupport
import qualified Data.Vector.Unboxed as Vec

import Criterion.Main

main = do
    let count = 64000
    
    mwcSrc <- newGenIO
    mtSrc  <- newMTSrc
    stdSrc <- newStdSrc
    
    defaultMain
        [ bgroup "dists" 
            [ bgroup "MWC"          (dists mwcSrc       count)
            , bgroup "PureMT"       (dists mtSrc        count)
            , bgroup "StdGen"       (dists stdSrc       count)
            , bgroup "DevRandom"    (dists DevRandom    count)
            , bgroup "DevURandom"   (dists DevURandom   count)
            ]
        
        , bgroup "IO StdGen" 
            [ bench "randomRIO" $ do
                xs <- replicateM count (randomRIO (10,50))
                (sum' xs :: Double) `seq` return ()
        
            , bench "uniform A" $ do
                xs <- replicateM count (sampleFrom stdSrc (uniform 10 50))
                (sum' xs :: Double) `seq` return () :: IO ()
            , bench "uniform B" $ do
                xs <- sampleFrom stdSrc (replicateM count (uniform 10 50))
                (sum' xs :: Double) `seq` return () :: IO ()
            ]
        
        , bgroup "pure StdGen"
            [ bgroup "Double"
                [ bench "getRandomRs" $ do
                    src <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) src
                    sum' (take count xs :: [Double]) `seq` return ()
                , bench "RVarT, State - sample replicateM" $ do
                    src <- newStdGen
                    let xs = evalState (runRVar (replicateM count (uniform 10 50)) StdRandom) src
                    (sum' xs :: Double) `seq` return ()
                , bench "RVarT, State - replicateM sample" $ do
                    src <- newStdGen
                    let xs = evalState (replicateM count (runRVar (uniform 10 50) StdRandom)) src
                    (sum' xs :: Double) `seq` return ()
                ]
            , bgroup "Int"
                [ bench "getRandomRs" $ do
                    src <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) src
                    sum' (take count xs :: [Int]) `seq` return ()
                , bench "RVarT, State - sample replicateM" $ do
                    src <- newStdGen
                    let xs = evalState (runRVar (replicateM count (uniform 10 50)) StdRandom) src
                    (sum' xs :: Int) `seq` return ()
                , bench "RVarT, State - replicateM sample" $ do
                    src <- newStdGen
                    let xs = evalState (replicateM count (runRVar (uniform 10 50) StdRandom)) src
                    (sum' xs :: Int) `seq` return ()
                ]
            ]
        
        , bgroup "MWC"
            [ bgroup "stdUniform"
                [ bench "Double" $ do
                    src <- newGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform src)
                    sum' (xs :: [Double]) `seq` return ()
                , bench "Int" $ do
                    src <- newGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform src)
                    sum' (xs :: [Int]) `seq` return ()
                ]
            , bgroup "uniform"
                [ bench "Double" $ do
                    src <- newGenIO
                    us <- stToIO $ replicateM count (MWC.uniform src)
                    let xs = [(u - 0.5) * 20| u <- us]
                    sum' (xs :: [Double]) `seq` return ()
                ]
--            , bgroup "normal"
--                [ bench "Double" $ do
--                    src <- newGenIO
--                    xs <- stToIO $ replicateM count (MWC.normal src)
--                    sum' (xs :: [Double]) `seq` return ()
--                ]
            , bgroup "uniformVector"
                [ bench "Double" $ do
                    src <- newGenIO
                    xs <- stToIO $ MWC.uniformVector src count 
                    -- unboxed, so don't need to force it, but we sum it
                    -- anyway to make the comparison fair between other tests
                    (Vec.sum xs :: Double) `seq` return ()
                ]
            ]
        
        , bench "baseline sum" $ nf sum' [1..fromIntegral count :: Double]
            
        ]

dists src count =
    [ multiSuite src count "uniform"     (Uniform 10 50)
    , multiSuite src count "stdUniform"  StdUniform
    , multiSuite src count "poisson"     (Poisson 3 :: Num t => Poisson Double t)
    , multiSuite src count "binomial 10" (Binomial 10 (0.5 :: Float))
    , doubleSuite src count "stdNormal"   StdNormal
    , doubleSuite src count "exponential" (Exp  2)
    , doubleSuite src count "beta"        (Beta  2 5)
    , doubleSuite src count "gamma"       (Gamma 2 5)
    , doubleSuite src count "triangular"  (Triangular 2 5 14)
    , doubleSuite src count "rayleigh"    (Rayleigh 1.6)
    
    , bench "dirichlet" $ do
        xs <- sampleFrom src (dirichlet [1..fromIntegral count :: Double])
        sum' xs `seq` return () :: IO ()
        
    , bgroup "multinomial" 
        [ bgroup "many p"
            [ bench desc $ do
                xs <- sampleFrom src (multinomial [1..1e4 :: Double] (n :: Int))
                sum' xs `seq` return () :: IO ()
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        , bgroup "few p" 
            [bench desc $ do
                replicateM_ 1000 $ do
                    xs <- sampleFrom src (multinomial [1..10 :: Double] (n :: Int))
                    sum' xs `seq` return () :: IO ()
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        ]
       
    , bench "shuffle" $ do
        xs <- sampleFrom src (shuffle [1..count])
        sum' xs `seq` return () :: IO ()
    ]

multiSuite :: (Distribution d Double, Distribution d Int, RandomSource IO s) => s -> Int -> String -> (forall t. Num t => d t) -> Benchmark
multiSuite src count name dist = bgroup name
    [ doubleSuite src count "Double" dist
    , intSuite    src count "Int"    dist
    ]

doubleSuite :: (Distribution d Double, RandomSource IO s) => s -> Int -> String -> d Double -> Benchmark
doubleSuite = suite

intSuite :: (Distribution d Int, RandomSource IO s) => s -> Int -> String -> d Int -> Benchmark
intSuite = suite

suite :: (Storable t, Num t, Distribution d t, RandomSource IO s) => s -> Int -> String -> d t -> Benchmark
suite src count name var = bgroup name 
    [ bench "single sample" $ do
        x <- sampleFrom src var
        x `seq` return () :: IO ()

    -- Ideally, these would all be the same speed:
    , bench "sum of samples (implicit rvar)" $ do
        x <- sumM count (sampleFrom src var)
        x `seq` return () :: IO ()

    , bench "sum of samples (explicit rvar)" $ do
        x <- sumM count (sampleFrom src (rvar var))
        x `seq` return () :: IO ()

    , bench "sample of sum" $ do
        x <- sampleFrom src (sumM count (rvar var))
        x `seq` return () :: IO ()
    
    , bench "array of samples" $ do
        allocaArray count $ \ptr -> do
            sequence_
                [ do
                    x <- sampleFrom src var
        
                    pokeElemOff ptr offset x
                | offset <- [0 .. count-1]
                ]
            sumBuf count ptr
    
    , bench "RVarT IO arrays" $ do
        allocaArray count $ \ptr -> flip runRVarT src $ do
            sequence_
                [ do
                    x <- rvarT var
        
                    lift (pokeElemOff ptr offset x)
                | offset <- [0 .. count-1]
                ]
            lift (sumBuf count ptr)
    ]
