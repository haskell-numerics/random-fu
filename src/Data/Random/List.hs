{-
 -      ``Data/Random/List''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
    FlexibleContexts
  #-}

module Data.Random.List where

import Data.Random.RVar
import Data.Random.Source
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import GHC.IOBase

import qualified Data.Sequence as S

randomElement :: [a] -> RVarT m a
randomElement [] = error "randomElement: empty list!"
randomElement xs = do
    n <- uniform 0 (length xs - 1)
    return (xs !! n)

shuffle :: [a] -> RVarT m [a]
shuffle = shuffleSeq . S.fromList

shuffleSeq :: S.Seq a -> RVarT m [a]
shuffleSeq s = shuffle (S.length s) s
    where
        shuffle 0 _ = return []
        shuffle (n+1) s = do
            i <- uniform 0 n
            let (x, xs) = extract i s
            ys <- shuffle n xs
            return (x:ys)
        
        extract n s = case S.splitAt n s of
            (l,r) -> case S.viewl r of
                x S.:< r  -> (x, l S.>< r)

lazyShuffleFrom :: (RandomSource IO s) => s -> [a] -> IO [a]
lazyShuffleFrom src = lazyShuffleSeqFrom src . S.fromList

lazyShuffleSeqFrom :: (RandomSource IO s) => s -> S.Seq a -> IO [a]
lazyShuffleSeqFrom src s = shuffle (S.length s) s
    where
        shuffle 0     _  = return []
        shuffle (n+1) s 
            | S.null s = return []
            | otherwise = do
                i <- runRVar (uniform 0 n) src
                let (x, xs) = extract i s
                ys <- unsafeInterleaveIO (shuffle n xs)
                return (x:ys)
        
        extract n s = case S.splitAt n s of
            (l,r) -> case S.viewl r of
                x S.:< r  -> (x, l S.>< r)
