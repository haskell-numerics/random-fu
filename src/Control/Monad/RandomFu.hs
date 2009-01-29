{-
 -      ``Control/Monad/RandomFu''
 - 
 -  Needs much cleanup...
 -  Also, don't really know which I want...
 -      ByteString, Lazy ByteString, or [Word8]?
 -  And do I want to provide splitting primitives?
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances
  #-}

module Control.Monad.RandomFu where

import Data.ByteString (ByteString, pack, unpack, hGet)
import Data.List
import Data.Word
import Data.Int
import Control.Monad

import GHC.IOBase (unsafePerformIO)
import System.IO (openBinaryFile, IOMode(..))
import Data.Bits
import System.Random

class MonadRandom m => MonadRandomSeed m where
    getRandomState :: m ByteString
    setRandomState :: (Int -> Word8) -> m ()

class Monad m => MonadRandom m where
    -- |get the specified number of random (uniformly distributed) bytes
    getRandomBytes :: Int -> m ByteString

class Monad m => RandomSource m s where
    getRandomBytesFrom :: s -> Int -> m ByteString

class Distribution d t where
    computeDist :: Monad m => (Int -> m ByteString) -> d t -> m t

data DevRandom = DevRandom
{-# NOINLINE devRandom #-}
devRandom = unsafePerformIO (openBinaryFile "/dev/random" ReadMode)

instance RandomSource IO DevRandom where
    getRandomBytesFrom DevRandom n = hGet devRandom n

data StdRandom = StdRandom

instance RandomSource IO StdRandom where
    getRandomBytesFrom StdRandom n = do
        ints <- replicateM n (randomRIO (0, 255)) :: IO [Int]
        let bytes = map fromIntegral ints
        return (pack bytes)

instance Monad m => RandomSource m (Int -> m ByteString) where
    getRandomBytesFrom = id

data Uniform t = Uniform !t !t

instance Distribution Uniform Int8    where computeDist = computeIntegralUniformDist
instance Distribution Uniform Int16   where computeDist = computeIntegralUniformDist
instance Distribution Uniform Int32   where computeDist = computeIntegralUniformDist
instance Distribution Uniform Int64   where computeDist = computeIntegralUniformDist
instance Distribution Uniform Int     where computeDist = computeIntegralUniformDist
instance Distribution Uniform Integer where computeDist = computeIntegralUniformDist
instance Distribution Uniform Word8   where computeDist = computeIntegralUniformDist
instance Distribution Uniform Word16  where computeDist = computeIntegralUniformDist
instance Distribution Uniform Word32  where computeDist = computeIntegralUniformDist
instance Distribution Uniform Word64  where computeDist = computeIntegralUniformDist

-- quick & dirty test:
-- let x = getDistFrom DevRandom (Uniform (-100) (100) :: Uniform Int8) :: IO Int8 in mapM_ (\x -> putStrLn (replicate (x `div` 10) '*')) . map length . group . sort =<< replicateM 100000 x

computeNByteIntegerWhere :: (Monad m) =>
                            (Int -> m ByteString) -> Int -> (Integer -> Bool) -> m Integer
computeNByteIntegerWhere getRandomBytes n p = do
    initialBytes <- getRandomBytes n
    let mask = 256 ^ n - 1
    
    let compute x
            | p x       = return x
            | otherwise = do
                [newByte] <- liftM unpack (getRandomBytes 1)
                compute (mask .&. (x `shiftL` 8) .|. toInteger newByte)
        
    
    compute (concatBytes initialBytes)

computeIntegralUniformDist getRandomBytes (Uniform a b) = do
    let m = toInteger b - toInteger a
    
    let bytes = bytesNeeded m
        maxXpossible = (powersOf256 !! bytes) - 1
    
    x <- computeNByteIntegerWhere getRandomBytes bytes (\x -> maxXpossible - x > maxXpossible `mod` m)
    return (a + fromInteger (x `mod` m))

instance Distribution Uniform Float  where computeDist = computeRealFloatUniformDist
instance Distribution Uniform Double where computeDist = computeRealFloatUniformDist

computeRealFloatUniformDist getRandomBytes (Uniform 0 f@1) = do
    let bitsNeeded  = floatDigits f
        maxVal = 2^bitsNeeded
        (_, e) = decodeFloat f
    
    x <- computeIntegralUniformDist getRandomBytes (Uniform 0 maxVal)
    if x == 0
        then return 0
        else if x == maxVal
            then return 1
            else return (encodeFloat x (e-1))
    
computeRealFloatUniformDist getRandomBytes (Uniform a b) = do
    let m = b - a
    x <- computeRealFloatUniformDist getRandomBytes (Uniform 0 1)
    return (a + m * x)

hist :: Ord a => [a] -> [a] -> [[a]]
hist = hist' . sort
    where
        hist' []     ys = []
        hist' (x:xs) ys = case break (>x) ys of
            (as, bs) -> as : hist' xs bs

bytesNeeded x = case findIndex (> x) powersOf256 of
    Just x -> x
powersOf256 = iterate (256 *) 1

zeroBits :: (Bits a) => a
zeroBits = bit 0 .&. bit 1

concatBytes :: Bits a => ByteString -> a
concatBytes = concatBits fromIntegral . unpack

concatBits :: (Bits a, Bits b) => (a -> b) -> [a] -> b
concatBits f [] = zeroBits
concatBits f (x:xs) = f x .|. (concatBits f xs `shiftL` bitSize x)

getDist :: (Distribution d t, MonadRandom m) => d t -> m t
getDist = computeDist getRandomBytes
getDistFrom :: (RandomSource m s, Distribution d t) => s -> d t -> m t
getDistFrom src = computeDist (getRandomBytesFrom src)
