{-
 -      ``Data/Random/Source/PreFetch''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts
  #-}

module Data.Random.Source.PreFetch
    ( PreFetch
    , mkPrefetch
    ) where

import Data.Random.Source
import Control.Concurrent
import Data.StateRef
import Data.Channel
import Data.Word
import Control.Monad

data PreFetch = PreFetch 
    { preFetchDeficit :: TVar Int
    , preFetchQueue   :: TChan [Word8]
    , preFetchPid     :: ThreadId
    }

mkPrefetch :: RandomSource IO s => Int -> Int -> s -> IO PreFetch
mkPrefetch sz bs src = do
    def   <- newRef sz
    queue <- newChannel
    
    pid <- forkIO $ forever $ do
        d <- readRef def
        
        if (d > 0)
            then do
                xs <- getRandomBytesFrom src bs
                
                -- putStrLn ("deficit : " ++ show d)
                atomically $ do
                    writeChannel queue xs
                    modifyRef def pred
            else threadDelay 10000
    
    return $ PreFetch
        { preFetchDeficit = def
        , preFetchQueue   = queue
        , preFetchPid     = pid
        }

instance RandomSource IO PreFetch where
    getRandomBytesFrom pf@(PreFetch { preFetchQueue = queue, preFetchDeficit = ~def }) n = do
        xs <- readChannel queue
        case splitAt n xs of
            (xs, extra) -> do
                let count = length xs
                
                if null extra
                    then do
                        modifyRef def succ
                        rest <- getRandomBytesFrom pf (n - length xs)
                        return (xs ++ rest)
                    else do
                        unGetChannel queue extra
                        return xs
