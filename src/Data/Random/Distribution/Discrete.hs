{-
 -      ``Data/Random/Distribution/Discrete''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Discrete where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.List (randomElement)

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse, sequenceA))

import Data.List
import Data.Function

discrete :: Distribution (Discrete p) a => [(p,a)] -> RVar a
discrete ps = rvar (Discrete ps)

empirical :: (Num p, Ord a) => [a] -> Discrete p a
empirical xs = Discrete bins
    where bins = [ (genericLength bin, x)
                 | bin@(x:_) <- group (sort xs)
                 ]

newtype Discrete p a = Discrete [(p, a)]
    deriving (Eq, Show)

instance (Num p, Ord p, Distribution Uniform p) => Distribution (Discrete p) a where
    rvar (Discrete []) = fail "discrete distribution over empty set cannot be sampled"
    rvar (Discrete ds) = do
        let (ps, xs) = unzip ds
            cs = scanl1 (+) ps
        
        when (not (all (>=0) ps)) $ fail "invalid probability in discrete distribution"
        
        let totalWeight = last cs
        if  totalWeight <= 0
            -- if all events are "equally impossible", just pick an arbitrary one.
            then randomElement xs
            else do
                let getU = do
                        u <- uniform 0 totalWeight
                        -- reject 0; this causes integral weights to behave sensibly (although it 
                        -- potentially wastes up to 50% of all sampled integral values in the
                        -- case where totalWeight = 1) and is still valid for fractional weights
                        -- (it only prevents zero-probability events from ever occurring, which 
                        -- is reasonable).
                        if u == 0 then getU
                                  else return u
                u <- getU
                return $ head
                    [ x
                    | (c,x) <- zip cs xs
                    , c >= u
                    ]

instance Functor (Discrete p) where
    fmap f (Discrete ds) = Discrete [(p, f x) | (p, x) <- ds]

instance Foldable (Discrete p) where
    foldMap f (Discrete ds) = foldMap (f . snd) ds

instance Traversable (Discrete p) where
    traverse f (Discrete ds) = Discrete <$> traverse (\(p,e) -> (\e -> (p,e)) <$> f e) ds
    sequenceA  (Discrete ds) = Discrete <$> traverse (\(p,e) -> (\e -> (p,e)) <$>   e) ds

-- We want each subset of cases in fx derived from a given case 
-- in x to have the same relative weight as the set in x from whence they came.
instance Num p => Monad (Discrete p) where
    return x = Discrete [(1, x)]
    (Discrete x) >>= f = Discrete $ do
        (p, x) <- x
        
        let Discrete fx = f x
        (q, x) <- fx
        
        return (p * q, x)

instance Num p => Applicative (Discrete p) where
    pure = return
    (<*>) = ap

-- |Like 'fmap', but for the weights of a discrete distribution.
mapDiscreteWeights :: (p -> q) -> Discrete p e -> Discrete q e
mapDiscreteWeights f (Discrete ds) = Discrete [(f p, x) | (p, x) <- ds]

-- |Adjust all the weights of a discrete distribution so that they 
-- sum to unity.  If not possible, returns the original distribution 
-- unchanged.
normalizeDiscreteWeights :: (Fractional p) => Discrete p e -> Discrete p e
normalizeDiscreteWeights orig@(Discrete ds) = 
    -- For practical purposes the scale factor is strict anyway,
    -- so check if it's 0 or 1 and, if so, skip the actual scaling part.
    if ws `elem` [0,1]
        then orig
        else Discrete
                [ (w * scale, e)
                | (w, e) <- ds
                ] 
    where
        ws = sum (map fst ds)
        scale = recip ws

-- |Simplify a discrete distribution by combining equivalent events (the new
-- event will have a weight equal to the sum of all the originals).
collectDiscreteEvents :: (Ord e, Num p, Ord p) => Discrete p e -> Discrete p e
collectDiscreteEvents = collectDiscreteEventsBy compare sum head
        
-- |Simplify a discrete distribution by combining equivalent events (the new
-- event will have a weight equal to the sum of all the originals).
-- The comparator function is used to identify events to combine.  Once chosen,
-- the events and their weights are combined (independently) by the provided
-- weight and event aggregation functions.
collectDiscreteEventsBy :: (e -> e -> Ordering) -> ([p] -> p) -> ([e] -> e)-> Discrete p e -> Discrete p e
collectDiscreteEventsBy compareE sumWeights mergeEvents (Discrete ds) = 
    Discrete . map ((sumWeights *** mergeEvents) . unzip) . groupEvents . sortEvents $ ds
    
    where
        groupEvents = groupBy (\x y -> snd x `compareE` snd y == EQ)
        sortEvents  = sortBy (compareE `on` snd)
        
        weight (p,x)
            | p < 0     = error "negative probability in discrete distribution"
            | isNaN p   = error "NaN probability in discrete distribution"
            | otherwise = p
        event ((p,x):_) = x
        
        combine (ps, xs) = (sumWeights ps, mergeEvents xs)