{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

module Main where

import Data.Random
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Dirichlet
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Multinomial
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Rayleigh
import Data.Random.Distribution.Triangular

import System.Random.Stateful (runStateGen_)

import Control.DeepSeq (NFData)
import Control.Monad
import qualified Control.Monad.Random as CMR
import Control.Monad.ST
import Control.Monad.State
import Foreign
import System.Random hiding (uniform)
import qualified System.Random.MWC as MWC
import TestSupport

import Criterion.Main

main = do
    let count = 64000

    mwcGen <- newMWCGenIO
    mtGen  <- newMTGenM
    stdGen <- newStdGenM

    defaultMain
        [ bgroup "dists"
            [ bgroup "MWC"          (dists mwcGen       count)
            , bgroup "PureMT"       (dists mtGen        count)
            , bgroup "StdGen"       (dists stdGen       count)
            ]

        , bgroup "IO StdGen"
            [ bench "randomRIO" $ nfIO $ do
                sum' <$> replicateM count (randomRIO (10,50)) :: IO Double

            , bench "uniform A" $ nfIO $ do
                sum' <$> replicateM count (sampleFrom stdGen (uniform 10 50)) :: IO Double

            , bench "uniform B" $ nfIO $ do
                sum' <$> sampleFrom stdGen (replicateM count (uniform 10 50)) :: IO Double
            ]

        , bgroup "pure StdGen"
            [ bgroup "Double"
                [ bench "getRandomRs" $ nfIO $ do
                    gen <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) gen
                    return $ sum' (take count xs :: [Double])
                , bench "RVarT, State - sample replicateM" $ nfIO $ do
                    gen <- newStdGen
                    let xs = runStateGen_ gen $ runRVar (replicateM count (uniform 10 50))
                    return (sum' xs :: Double)
                , bench "RVarT, State - replicateM sample" $ nfIO $ do
                    gen <- newStdGen
                    let xs = runStateGen_ gen $ replicateM count . runRVar (uniform 10 50)
                    return (sum' xs :: Double)
                ]
            , bgroup "Int"
                [ bench "getRandomRs" $ nfIO $ do
                    gen <- newStdGen
                    let xs = CMR.evalRand (CMR.getRandomRs (10,50)) gen
                    return $ sum' (take count xs :: [Int])
                , bench "RVarT, State - sample replicateM" $ nfIO $ do
                    gen <- newStdGen
                    let xs = runStateGen_ gen $ runRVar (replicateM count (uniform 10 50))
                    return $ (sum' xs :: Int)
                , bench "RVarT, State - replicateM sample" $ nfIO $ do
                    gen <- newStdGen
                    let xs = runStateGen_ gen $ replicateM count . runRVar (uniform 10 50)
                    return $ (sum' xs :: Int)
                ]
            ]

        , bgroup "MWC"
            [ bgroup "stdUniform"
                [ bench "Double" $ nfIO $ do
                    gen <- newMWCGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform gen)
                    return $ sum' (xs :: [Double])
                , bench "Int" $ nfIO $ do
                    gen <- newMWCGenIO
                    xs <- stToIO $ replicateM count (MWC.uniform gen)
                    return $ sum' (xs :: [Int])
                ]
            , bgroup "uniform"
                [ bench "Double" $ nfIO $ do
                    gen <- newMWCGenIO
                    us <- stToIO $ replicateM count (MWC.uniform gen)
                    let xs = [(u - 0.5) * 20| u <- us]
                    return $ sum' (xs :: [Double])
                ]
--            , bgroup "normal"
--                [ bench "Double" $ nfIO $ do
--                    gen <- newGenIO
--                    xs <- stToIO $ replicateM count (MWC.normal gen)
--                    return $ sum' (xs :: [Double])
--                ]
            -- FIXME: uniformVector no longer works on Double
            -- , bgroup "uniformVector"
            --     [ bench "Double" $ nfIO $ do
            --         gen <- newGenIO
            --         xs <- stToIO $ MWC.uniformVector gen count
            --         -- unboxed, so don't need to force it, but we sum it
            --         -- anyway to make the comparison fair between other tests
            --         return $ (Vec.sum xs :: Double)
            --     ]
            ]

        , bench "baseline sum" $ nf sum' [1..fromIntegral count :: Double]

        ]

dists gen count =
    [ multiSuite gen count "uniform"     (Uniform 10 50)
    , multiSuite gen count "stdUniform"  StdUniform
    , multiSuite gen count "poisson"     (Poisson 3 :: Num t => Poisson Double t)
    , multiSuite gen count "binomial 10" (Binomial 10 (0.5 :: Float))
    , doubleSuite gen count "stdNormal"   StdNormal
    , doubleSuite gen count "exponential" (Exp  2)
    , doubleSuite gen count "beta"        (Beta  2 5)
    , doubleSuite gen count "gamma"       (Gamma 2 5)
    , doubleSuite gen count "triangular"  (Triangular 2 5 14)
    , doubleSuite gen count "rayleigh"    (Rayleigh 1.6)

    , bench "dirichlet" $ nfIO $ do
        sum' <$> sampleFrom gen (dirichlet [1..fromIntegral count :: Double])

    , bgroup "multinomial"
        [ bgroup "many p"
            [ bench desc $ nfIO $ do
                sum' <$> sampleFrom gen (multinomial [1..1e4 :: Double] (n :: Int))
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        , bgroup "few p"
            [bench desc $ nfIO $ do
                replicateM_ 1000 $ do
                    sum' <$> sampleFrom gen (multinomial [1..10 :: Double] (n :: Int))
            | (desc, n) <- [("small n", 10), ("medium n", 10^4), ("large n", 10^8)]
            ]
        ]

    , bench "shuffle" $ nfIO $ do
        sum' <$> sampleFrom gen (shuffle [1..count])
    ]

multiSuite :: (Distribution d Double, Distribution d Int, StatefulGen g IO) => g -> Int -> String -> (forall t. Num t => d t) -> Benchmark
multiSuite gen count name dist = bgroup name
    [ doubleSuite gen count "Double" dist
    , intSuite    gen count "Int"    dist
    ]

doubleSuite :: (Distribution d Double, StatefulGen g IO) => g -> Int -> String -> d Double -> Benchmark
doubleSuite = suite

intSuite :: (Distribution d Int, StatefulGen g IO) => g -> Int -> String -> d Int -> Benchmark
intSuite = suite

suite :: (Storable t, Num t, Distribution d t, NFData t, StatefulGen g IO) => g -> Int -> String -> d t -> Benchmark
suite gen count name var = bgroup name
    [ bench "single sample" $ nfIO $ do
        sampleFrom gen var

    -- Ideally, these would all be the same speed:
    , bench "sum of samples (implicit rvar)" $ nfIO $ do
        sumM count (sampleFrom gen var)

    , bench "sum of samples (explicit rvar)" $ nfIO $ do
        sumM count (sampleFrom gen (rvar var))

    , bench "sample of sum" $ nfIO $ do
        sampleFrom gen (sumM count (rvar var))

    , bench "array of samples" $ nfIO $ do
        allocaArray count $ \ptr -> do
            sequence_
                [ do
                    x <- sampleFrom gen var

                    pokeElemOff ptr offset x
                | offset <- [0 .. count-1]
                ]
            sumBuf count ptr

    , bench "RVarT IO arrays" $ nfIO $ do
        allocaArray count $ \ptr -> flip runRVarT gen $ do
            sequence_
                [ do
                    x <- rvarT var

                    lift (pokeElemOff ptr offset x)
                | offset <- [0 .. count-1]
                ]
            lift (sumBuf count ptr)
    ]
