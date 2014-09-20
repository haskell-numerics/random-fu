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

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData)
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
            [ bench "randomRIO" $ nfIO $ do
                sum' <$> replicateM count (randomRIO (10,50)) :: IO Double

            , bench "uniform A" $ nfIO $ do
                sum' <$> replicateM count (sampleFrom stdSrc (uniform 10 50)) :: IO Double

            , bench "uniform B" $ nfIO $ do
                sum' <$> sampleFrom stdSrc (replicateM count (uniform 10 50)) :: IO Double
            ]

        , bgroup "pure StdGen"
            [ bgroup "Double"
                [ bench "getRandomRs" $ nfIO $ do
                    src <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) src
                    return $ sum' (take count xs :: [Double])
                , bench "RVarT, State - sample replicateM" $ nfIO $ do
                    src <- newStdGen
                    let xs = evalState (runRVar (replicateM count (uniform 10 50)) StdRandom) src
                    return (sum' xs :: Double)
                , bench "RVarT, State - replicateM sample" $ nfIO $ do
                    src <- newStdGen
                    let xs = evalState (replicateM count (runRVar (uniform 10 50) StdRandom)) src
                    return (sum' xs :: Double)
                ]
            , bgroup "Int"
                [ bench "getRandomRs" $ nfIO $ do
                    src <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) src
                    return $ sum' (take count xs :: [Int])
                , bench "RVarT, State - sample replicateM" $ nfIO $ do
                    src <- newStdGen
                    let xs = evalState (runRVar (replicateM count (uniform 10 50)) StdRandom) src
                    return $ (sum' xs :: Int)
                , bench "RVarT, State - replicateM sample" $ nfIO $ do
                    src <- newStdGen
                    let xs = evalState (replicateM count (runRVar (uniform 10 50) StdRandom)) src
                    return $ (sum' xs :: Int)
                ]
            ]

        , bgroup "MWC"
            [ bgroup "stdUniform"
                [ bench "Double" $ nfIO $ do
                    src <- newGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform src)
                    return $ sum' (xs :: [Double])
                , bench "Int" $ nfIO $ do
                    src <- newGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform src)
                    return $ sum' (xs :: [Int])
                ]
            , bgroup "uniform"
                [ bench "Double" $ nfIO $ do
                    src <- newGenIO
                    us <- stToIO $ replicateM count (MWC.uniform src)
                    let xs = [(u - 0.5) * 20| u <- us]
                    return $ sum' (xs :: [Double])
                ]
--            , bgroup "normal"
--                [ bench "Double" $ nfIO $ do
--                    src <- newGenIO
--                    xs <- stToIO $ replicateM count (MWC.normal src)
--                    return $ sum' (xs :: [Double])
--                ]
            , bgroup "uniformVector"
                [ bench "Double" $ nfIO $ do
                    src <- newGenIO
                    xs <- stToIO $ MWC.uniformVector src count
                    -- unboxed, so don't need to force it, but we sum it
                    -- anyway to make the comparison fair between other tests
                    return $ (Vec.sum xs :: Double)
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

    , bench "dirichlet" $ nfIO $ do
        sum' <$> sampleFrom src (dirichlet [1..fromIntegral count :: Double])

    , bgroup "multinomial"
        [ bgroup "many p"
            [ bench desc $ nfIO $ do
                sum' <$> sampleFrom src (multinomial [1..1e4 :: Double] (n :: Int))
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        , bgroup "few p"
            [bench desc $ nfIO $ do
                replicateM_ 1000 $ do
                    sum' <$> sampleFrom src (multinomial [1..10 :: Double] (n :: Int))
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        ]

    , bench "shuffle" $ nfIO $ do
        sum' <$> sampleFrom src (shuffle [1..count])
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

suite :: (Storable t, Num t, Distribution d t, NFData t, RandomSource IO s) => s -> Int -> String -> d t -> Benchmark
suite src count name var = bgroup name
    [ bench "single sample" $ nfIO $ do
        sampleFrom src var

    -- Ideally, these would all be the same speed:
    , bench "sum of samples (implicit rvar)" $ nfIO $ do
        sumM count (sampleFrom src var)

    , bench "sum of samples (explicit rvar)" $ nfIO $ do
        sumM count (sampleFrom src (rvar var))

    , bench "sample of sum" $ nfIO $ do
        sampleFrom src (sumM count (rvar var))

    , bench "array of samples" $ nfIO $ do
        allocaArray count $ \ptr -> do
            sequence_
                [ do
                    x <- sampleFrom src var

                    pokeElemOff ptr offset x
                | offset <- [0 .. count-1]
                ]
            sumBuf count ptr

    , bench "RVarT IO arrays" $ nfIO $ do
        allocaArray count $ \ptr -> flip runRVarT src $ do
            sequence_
                [ do
                    x <- rvarT var

                    lift (pokeElemOff ptr offset x)
                | offset <- [0 .. count-1]
                ]
            lift (sumBuf count ptr)
    ]
