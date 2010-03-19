module TestSupport where

import System.Random.Mersenne.Pure64
import System.Random.MWC
import Data.StateRef
import Control.Monad (forever)
import Control.Monad.ST

type Src = IORef PureMT
getTestSource = do
    mt <- newPureMT
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

nTimes Nothing  x = forever x
nTimes (Just n) x = go n
    where
        go 0 = return ()
        go (n+1) = x >> go n
