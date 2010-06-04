{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

    -- |--------------------------------------------------------------
    --            This is the BIRTHDAY SPACINGS TEST                --
    -- Choose m birthdays in a year of n days.  List the spacings   --
    -- between the birthdays.  If j is the number of values that    --
    -- occur more than once in that list, then j is asymptotically  --
    -- Poisson distributed with mean m^3/(4n).  Experience shows n  --
    -- must be quite large, say n>=2^18, for comparing the results  --
    -- to the Poisson distribution with that mean.  This test uses  --
    -- n=2^24 and m=2^9,  so that the underlying distribution for j --
    -- is taken to be Poisson with lambda=2^27/(2^26)=2.  A sample  --
    -- of 500 j's is taken, and a chi-square goodness of fit test   --
    -- provides a p value.  The first test uses bits 1-24 (counting --
    -- from the left) from integers in the specified file.          --
    --   Then the file is closed and reopened. Next, bits 2-25 are  --
    -- used to provide birthdays, then 3-26 and so on to bits 9-32. --
    -- Each set of bits provides a p-value, and the nine p-values   --
    -- provide a sample for a KSTEST.                               --
    --                                                              --
    ------------------------------------------------------------------
    --  Commentary and interpretations:
    -- 
    -- This definition is ambiguous, and not sufficient to actually
    -- implement the test as far as I can tell.  In line 1, "List
    -- the spacings between the birthdays", does this mean to sort
    -- the full list and then compute the differences between 
    -- successive entries?  I assume so.
    --
    -- Is the collection of birthdays regarded as a set or a multiset?
    -- That is, does the list of spacings include zeroes?  I assume so.
    -- 
    -- Is j the number of _distinct_ values?  Again, I assume so.
    -- (As opposed to the number of elements of the list of intervals
    -- whose values are repeated).  E.G. does [1,1,1,1] count
    -- as one repeated value or four?
    -- 
    -- Searches on the web are hardly illuminating here.  The vast
    -- majority of "explanations" of the test contradict each other
    -- and this one.
    -- 

module Test.DieHard.Birthday where

import Data.List
import Data.Random
import Data.Random.Distribution.Poisson

n = 2^24
m = 2^9

-- |A simple list operation that probably ought to be in Data.List:
-- Count the number of elements of a list that satisfy a predicate.
count :: Num b => (a -> Bool) -> [a] -> b
count p = foldr (\x c -> if p x then c + 1 else c) 0

-- |Sort a list and compute the differences between successive elements.
spacings :: (Ord a, Num a) => [a] -> [a]
spacings xs = zipWith subtract sorted (tail sorted)
    where sorted = sort xs

-- |Count the number of values that occur multiple times in a list.  This
-- counts the distinct values, not the occurrences.
multiples :: (Ord a, Num b) => [a] -> b
multiples xs = count (not.null.drop 1) (group (sort xs))

-- |Computes the \"Birthday Metric\": The number of distinct spacings that
-- occur more than once between adjacent elements in a sorted list of birthdays.
-- This is my interpretation of the specification of the birthday spacings test 
-- in the diehard battery of tests.  If I discover my interpretation to be 
-- wrong, this definition will change.
--
-- The input list need not be sorted - the sorting will be done by this 
-- function.
--
-- In the language of the test's description, this computes 'j' given the
-- list of birthdays (I believe).
birthday :: (Ord a, Num a, Num b) => [a] -> b
birthday xs = multiples (spacings xs)

-- |@Birthday n m@ is the expected distribution of 
-- > fmap birthday (replicateM m (uniform 1 n))
data Birthday a b = Birthday a a
    deriving (Eq, Show)

instance (Real a, Distribution (Poisson Double) b) => Distribution (Birthday a) b where
    rvarT (Birthday n m) = rvarT (Poisson lambda)
        where lambda = realToFrac m^3 / (4*realToFrac n) :: Double

instance (Real a, CDF (Poisson Double) b) => CDF (Birthday a) b where
    cdf (Birthday n m) = cdf (Poisson lambda)
        where lambda = realToFrac m^3 / (4*realToFrac n) :: Double
