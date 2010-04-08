module TestSupport where

import System.Random.Mersenne.Pure64
import System.Random.MWC
import Data.StateRef
import Control.Monad (forever)
import Control.Monad.ST
import Foreign
import System.Random

-- type Src = IORef PureMT
-- getTestSource = do
--     mt <- newPureMT
--     newReference mt :: IO Src

type Src = IORef StdGen
getTestSource = do
    mt <- newStdGen
    newReference mt :: IO Src

-- type Src = Gen RealWorld
-- getTestSource :: IO Src
-- getTestSource = stToIO create

sumM n x = go n 0
    where
        go    0  s = return s
        go (n+1) s = do
            x <- x
            go n $! (x + s)

sumBuf :: Int -> Ptr Double -> IO Double
sumBuf bufSz ptr = go bufSz 0
    where
        go 0     s = return s
        go (sz+8) s = do
            x <- peekByteOff ptr sz :: IO Double
            go sz $! (x + s)
