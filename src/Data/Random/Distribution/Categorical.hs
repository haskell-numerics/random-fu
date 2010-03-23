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

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse, sequenceA))

import Data.List
import Data.Function

-- |Construct a 'Categorical' distribution from a list of probabilities
-- and categories, where the probabilities all sum to 1.
categorical :: Distribution (Categorical p) a => [(p,a)] -> RVar a
categorical ps = rvar (Categorical ps)

-- | Construct a 'Categorical' distribution from a list of weighted categories,
-- where the weights do not necessarily sum to 1.
{-# INLINE weightedCategorical #-}
weightedCategorical :: (Fractional p) => [(p,a)] -> Categorical p a
weightedCategorical = normalizeCategoricalPs . Categorical

-- |Construct a 'Categorical' distribution from a list of observed outcomes.
-- Equivalent events will be grouped and counted, and the probabilities of each
-- event in the returned distribution will be proportional to the number of 
-- occurrences of that event.
empirical :: (Fractional p, Ord a) => [a] -> Categorical p a
empirical xs = normalizeCategoricalPs (Categorical bins)
    where bins = [ (genericLength bin, x)
                 | bin@(x:_) <- group (sort xs)
                 ]

-- |Categorical distribution; a list of events with corresponding probabilities.
-- The sum of the probabilities must be 1, and no event should have a zero 
-- or negative probability (at least, at time of sampling; very clever users
-- can do what they want with the numbers before sampling, just make sure 
-- that if you're one of those clever ones, you normalize before sampling).
newtype Categorical p a = Categorical [(p, a)]
    deriving (Eq, Show)

instance (Fractional p, Ord p, Distribution StdUniform p) => Distribution (Categorical p) a where
    rvar (Categorical []) = fail "categorical distribution over empty set cannot be sampled"
    rvar (Categorical ds) = do
        let (ps, xs) = unzip ds
            cs = scanl1 (+) ps
        
        u <- stdUniform
        getEvent u cs xs
        
        where
            -- Implicit epsilon, to deal with the facts that
            --  a) we don't know whether the type of the probabilities has exact addition/subtraction/etc.,
            --     but given the combination of @Fractional@ and @Distribution StdUniform@ in the context,
            --     we have strong reason to suspect it at least does not have exact division/multiplication,
            --     so the probabilities may not sum to exactly 1 if they arose from any non-trivial computation.
            --  b) we don't know what its ULP might be, if it even has one
            --
            -- Note that this particular setup relies heavily on the assumption that all 
            -- c's lie in the range 0 < c <= 1
            --
            -- This corresponds to an epsilon of 100 ULPs at whatever 
            -- resolution the type has near 1, I think.
            x >~ y  = (100 + (x - y) > 100)
            
            getEvent u = go 0
                where
                    go lastC [] _   = fail ("categorical distribution sampling error: probabilities do not sum to one (lastC == " ++ show lastC ++ ")")
                    go lastC (c:cs) (x:xs)
                        | c < lastC = fail "categorical distribution sampling error: negative probability for an event!"
                        | u >~ c    = go c cs xs
                        | otherwise = return x

instance Functor (Categorical p) where
    fmap f (Categorical ds) = Categorical [(p, f x) | ~(p, x) <- ds]

instance Foldable (Categorical p) where
    foldMap f (Categorical ds) = foldMap (f . snd) ds

instance Traversable (Categorical p) where
    traverse f (Categorical ds) = Categorical <$> traverse (\(p,e) -> (\e -> (p,e)) <$> f e) ds
    sequenceA  (Categorical ds) = Categorical <$> traverse (\(p,e) -> (\e -> (p,e)) <$>   e) ds

instance Fractional p => Monad (Categorical p) where
    return x = Categorical [(1, x)]
    
    -- I'm not entirely sure whether this is a valid form of failure; see next
    -- set of comments.
    fail _ = Categorical []
    
    -- Should the normalize step be included here, or should normalization
    -- be assumed?  It seems like there is (at least) 1 valid situation where
    -- non-normal results would arise:  the distribution being modeled is 
    -- "conditional" and some event arose that contradicted the assumed 
    -- condition and thus was eliminated ('f' returned an empty or 
    -- zero-probability consequent, possibly by 'fail'ing).
    -- 
    -- It seems reasonable to continue in such circumstances, but should there
    -- be any renormalization?  If so, does it make a difference when that 
    -- renormalization is done?  I'm pretty sure it does, actually.  So, the
    -- normalization will be omitted here for now, as it's easier for the
    -- user (who really better know what they mean if they're returning
    -- non-normalized probability anyway) to normalize explicitly than to
    -- undo any normalization that was done automatically.
    (Categorical x) >>= f = {- normalizeCategoricalPs . -} Categorical $ do
        (p, x) <- x
        
        let Categorical fx = f x
        (q, x) <- fx
        
        return (p * q, x)

instance Fractional p => Applicative (Categorical p) where
    pure = return
    (<*>) = ap

-- |Like 'fmap', but for the probabilities of a categorical distribution.
mapCategoricalPs :: (p -> q) -> Categorical p e -> Categorical q e
mapCategoricalPs f (Categorical ds) = Categorical [(f p, x) | (p, x) <- ds]

-- |Adjust all the weights of a categorical distribution so that they 
-- sum to unity.
normalizeCategoricalPs :: (Fractional p) => Categorical p e -> Categorical p e
normalizeCategoricalPs orig@(Categorical ds) = 
    -- For practical purposes the scale factor is strict anyway,
    -- so check if the total probability is 1 and, if so, skip 
    -- the actual scaling part.
    --
    -- Along the way, discard any zero-probability events.
    if null ds || ps =~ 1
        then orig
        else Categorical
                [ (p * scale, e)
                | (p, e) <- ds
                , p /= 0
                ] 
    where
        ps = foldl1' (+) (map fst ds)
        scale = recip ps
        
        -- Using same implicit-epsilon trick as in Distribution instance
        -- (see comments there)
        x =~ y  = (100 + (x-y) == 100)


-- |Simplify a categorical distribution by combining equivalent categories (the new
-- category will have a probability equal to the sum of all the originals).
collectEvents :: (Ord e, Num p, Ord p) => Categorical p e -> Categorical p e
collectEvents = collectEventsBy compare ((sum *** head) . unzip)
        
-- |Simplify a categorical distribution by combining equivalent events (the new
-- event will have a weight equal to the sum of all the originals).
-- The comparator function is used to identify events to combine.  Once chosen,
-- the events and their weights are combined by the provided probability and
-- event aggregation function.
collectEventsBy :: (e -> e -> Ordering) -> ([(p,e)] -> (p,e))-> Categorical p e -> Categorical p e
collectEventsBy compareE combine (Categorical ds) = 
    Categorical . map combine . groupEvents . sortEvents $ ds
    where
        groupEvents = groupBy (\x y -> snd x `compareE` snd y == EQ)
        sortEvents  = sortBy (compareE `on` snd)
