{-
 -      ``Data/Random/Internal/Find''
 -  Utilities for searching fractional domains.  Needs cleanup, testing,
 -  and such.  Used for constructing generic ziggurats.
 -}

module Data.Random.Internal.Find where

findMax :: (Fractional a, Ord a) => (a -> Bool) -> a
findMax p = negate (findMin (p.negate))

-- |Given an upward-closed predicate on an ordered Fractional type,
-- find the smallest value satisfying the predicate.
findMin :: (Fractional a, Ord a) => (a -> Bool) -> a
findMin = findMinFrom 0 1

-- |Given an upward-closed predicate on an ordered Fractional type,
-- find the smallest value satisfying the predicate.  Starts at the
-- specified point with the specified stepsize, performs an exponential
-- search out from there until it finds an interval bracketing the
-- change-point of the predicate, and then performs a bisection search
-- to isolate the change point.  Note that infinitely-divisible domains 
-- such as 'Rational' cannot be searched by this function because it does
-- not terminate until it reaches a point where further subdivision of the
-- interval has no effect.
findMinFrom :: (Fractional a, Ord a) => a -> a -> (a -> Bool) -> a
findMinFrom z0 0 p = findMinFrom z0 1 p
findMinFrom z0 step1 p
    | p z0      = descend (z0-step1) z0
    | otherwise = fixZero (ascend z0 (z0+step1))
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
            | otherwise = ascend x $! 2*x-z0
        
        -- preconditions:
        -- p h
        -- x < h <= 0
        descend x h 
            | p x       = (descend $! 2*x-z0) x
            | otherwise = bisect x h
        
        -- preconditions:
        -- not (p l)
        -- p h
        -- l <= h
        bisect l h 
            | l /< h    = h
            | l /< mid || mid /< h
            = if p mid then mid else h
            | p mid     = bisect l mid
            | otherwise = bisect mid h
            where 
                a /< b = not (a < b)
                mid = (l+h)*0.5
