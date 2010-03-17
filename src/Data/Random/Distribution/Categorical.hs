{-
 -      ``Data/Random/Distribution/Categorical''
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Categorical where

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

categorical :: Distribution (Categorical p) a => [(p,a)] -> RVar a
categorical ps = rvar (Categorical ps)

empirical :: (Num p, Ord a) => [a] -> Categorical p a
empirical xs = Categorical bins
    where bins = [ (genericLength bin, x)
                 | bin@(x:_) <- group (sort xs)
                 ]

newtype Categorical p a = Categorical [(p, a)]
    deriving (Eq, Show)

instance (Num p, Ord p, Distribution Uniform p) => Distribution (Categorical p) a where
    rvar (Categorical []) = fail "categorical distribution over empty set cannot be sampled"
    rvar (Categorical ds) = do
        let (ps, xs) = unzip ds
            cs = scanl1 (+) ps
        
        when (not (all (>=0) ps)) $ fail "invalid probability in categorical distribution"
        
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

instance Functor (Categorical p) where
    fmap f (Categorical ds) = Categorical [(p, f x) | (p, x) <- ds]

instance Foldable (Categorical p) where
    foldMap f (Categorical ds) = foldMap (f . snd) ds

instance Traversable (Categorical p) where
    traverse f (Categorical ds) = Categorical <$> traverse (\(p,e) -> (\e -> (p,e)) <$> f e) ds
    sequenceA  (Categorical ds) = Categorical <$> traverse (\(p,e) -> (\e -> (p,e)) <$>   e) ds

-- We want each subset of cases in fx derived from a given case 
-- in x to have the same relative weight as the set in x from whence they came.
instance Num p => Monad (Categorical p) where
    return x = Categorical [(1, x)]
    (Categorical x) >>= f = Categorical $ do
        (p, x) <- x
        
        let Categorical fx = f x
        (q, x) <- fx
        
        return (p * q, x)

instance Num p => Applicative (Categorical p) where
    pure = return
    (<*>) = ap

-- |Like 'fmap', but for the weights of a categorical distribution.
mapDiscreteWeights :: (p -> q) -> Categorical p e -> Categorical q e
mapDiscreteWeights f (Categorical ds) = Categorical [(f p, x) | (p, x) <- ds]

-- |Adjust all the weights of a categorical distribution so that they 
-- sum to unity.  If not possible, returns the original distribution 
-- unchanged.
normalizeDiscreteWeights :: (Fractional p) => Categorical p e -> Categorical p e
normalizeDiscreteWeights orig@(Categorical ds) = 
    -- For practical purposes the scale factor is strict anyway,
    -- so check if the total weight is 0 or 1 and, if so, skip 
    -- the actual scaling part.
    if null ds || ws `elem` [0,1]
        then orig
        else Categorical
                [ (w * scale, e)
                | (w, e) <- ds
                ] 
    where
        ws = foldl1' (+) (map fst ds)
        scale = recip ws

-- |Simplify a categorical distribution by combining equivalent categories (the new
-- category will have a weight equal to the sum of all the originals).
collectDiscreteEvents :: (Ord e, Num p, Ord p) => Categorical p e -> Categorical p e
collectDiscreteEvents = collectDiscreteEventsBy compare sum head
        
-- |Simplify a categorical distribution by combining equivalent events (the new
-- event will have a weight equal to the sum of all the originals).
-- The comparator function is used to identify events to combine.  Once chosen,
-- the events and their weights are combined (independently) by the provided
-- weight and event aggregation functions.
collectDiscreteEventsBy :: (e -> e -> Ordering) -> ([p] -> p) -> ([e] -> e)-> Categorical p e -> Categorical p e
collectDiscreteEventsBy compareE sumWeights mergeEvents (Categorical ds) = 
    Categorical . map ((sumWeights *** mergeEvents) . unzip) . groupEvents . sortEvents $ ds
    
    where
        groupEvents = groupBy (\x y -> snd x `compareE` snd y == EQ)
        sortEvents  = sortBy (compareE `on` snd)
        
        weight (p,x)
            | p < 0     = error "negative probability in categorical distribution"
            | isNaN p   = error "NaN probability in categorical distribution"
            | otherwise = p
        event ((p,x):_) = x
        
        combine (ps, xs) = (sumWeights ps, mergeEvents xs)