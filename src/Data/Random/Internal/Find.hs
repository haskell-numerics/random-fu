{-
 -      ``Data/Random/Internal/Find''
 -  Utilities for searching fractional domains.  Needs cleanup, testing,
 -  and such.
 -}

module Data.Random.Internal.Find where

findMax :: (Fractional a, Ord a) => (a -> Bool) -> a
findMax p = negate (findMin (p.negate))

findMaxFrom :: (Fractional a, Ord a) => a -> a -> (a -> Bool) -> a
findMaxFrom z 0 p = findMaxFrom z 1 p
findMaxFrom z step1 p = findMinFrom z (negate step1) p

-- |Given an upward-closed predicate on an ordered Fractional type,
-- find the smallest value satisfying the predicate.
findMin :: (Fractional a, Ord a) => (a -> Bool) -> a
findMin = findMinFrom 0 1

findMinFrom :: (Fractional a, Ord a) => a -> a -> (a -> Bool) -> a
findMinFrom z 0 p = findMinFrom z 1 p
findMinFrom z step1 p
    | p z   = descend (z-step1) z
    | otherwise
    = fixZero (ascend z (z+step1))
    where
        -- eliminate negative zero, which, in many domains, is technically
        -- a feasible answer
        fixZero 0 = 0
        fixZero z = z
        
        -- preconditions:
        -- not (p l)
        -- 0 <= l < x
        ascend l x 
            | p x       = bisect l x
            | otherwise = ascend x $! 2*x-z
        
        -- preconditions:
        -- p h
        -- x < h <= 0
        descend x h 
            | p x       = (descend $! 2*x-z) x
            | otherwise = bisect x h
        
        -- preconditions:
        -- not (p l)
        -- p h
        -- l <= h
        bisect l h 
            | l >= h    = h
            | l >= mid || mid >= h
            = if p mid then mid else h
            | p mid     = bisect l mid
            | otherwise = bisect mid h
            where 
                a >= b = not (a < b)
                mid = (l+h)*0.5
