{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts,
    CPP
  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Random.Distribution.Categorical
    ( Categorical
    , categorical, categoricalT
    , weightedCategorical, weightedCategoricalT
    , fromList, toList, totalWeight, numEvents
    , fromWeightedList, fromObservations
    , mapCategoricalPs, normalizeCategoricalPs
    , collectEvents, collectEventsBy
    ) where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Foldable (Foldable(foldMap))
import Data.STRef
import Data.Traversable (Traversable(traverse, sequenceA))

import Data.List
import Data.Function
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- |Construct a 'Categorical' random variable from a list of probabilities
-- and categories, where the probabilities all sum to 1.
categorical :: (Num p, Distribution (Categorical p) a) => [(p,a)] -> RVar a
categorical = rvar . fromList

-- |Construct a 'Categorical' random process from a list of probabilities 
-- and categories, where the probabilities all sum to 1.
categoricalT :: (Num p, Distribution (Categorical p) a) => [(p,a)] -> RVarT m a
categoricalT = rvarT . fromList

-- |Construct a 'Categorical' random variable from a list of weights
-- and categories. The weights do /not/ have to sum to 1.
weightedCategorical :: (Fractional p, Eq p, Distribution (Categorical p) a) => [(p,a)] -> RVar a
weightedCategorical = rvar . fromWeightedList

-- |Construct a 'Categorical' random process from a list of weights 
-- and categories. The weights do /not/ have to sum to 1.
weightedCategoricalT :: (Fractional p, Eq p, Distribution (Categorical p) a) => [(p,a)] -> RVarT m a
weightedCategoricalT = rvarT . fromWeightedList

-- | Construct a 'Categorical' distribution from a list of weighted categories.
{-# INLINE fromList #-}
fromList :: (Num p) => [(p,a)] -> Categorical p a
fromList xs = Categorical (V.fromList (scanl1 f xs))
    where f (p0, _) (p1, y) = (p0 + p1, y)

{-# INLINE toList #-}
toList :: (Num p) => Categorical p a -> [(p,a)]
toList (Categorical ds) = V.foldr' g [] ds
    where
        g x [] = [x]
        g x@(p0,_) ((p1, y):xs) = x : (p1-p0,y) : xs

totalWeight :: Num p => Categorical p a -> p
totalWeight (Categorical ds)
    | V.null ds = 0
    | otherwise = fst (V.last ds)

numEvents :: Categorical p a -> Int
numEvents (Categorical ds) = V.length ds

-- |Construct a 'Categorical' distribution from a list of weighted categories, 
-- where the weights do not necessarily sum to 1.
fromWeightedList :: (Fractional p, Eq p) => [(p,a)] -> Categorical p a
fromWeightedList = normalizeCategoricalPs . fromList

-- |Construct a 'Categorical' distribution from a list of observed outcomes.
-- Equivalent events will be grouped and counted, and the probabilities of each
-- event in the returned distribution will be proportional to the number of 
-- occurrences of that event.
fromObservations :: (Fractional p, Eq p, Ord a) => [a] -> Categorical p a
fromObservations = fromWeightedList . map (genericLength &&& head) . group . sort

-- The following description refers to the public interface.  For those reading
-- the code, in the actual implementation Categorical is stored as a vector of
-- (cumulative-probability, value) pairs, so that sampling can take advantage of
-- binary search.

-- |Categorical distribution; a list of events with corresponding probabilities.
-- The sum of the probabilities must be 1, and no event should have a zero 
-- or negative probability (at least, at time of sampling; very clever users
-- can do what they want with the numbers before sampling, just make sure 
-- that if you're one of those clever ones, you at least eliminate negative 
-- weights before sampling).
newtype Categorical p a = Categorical (V.Vector (p, a))
    deriving Eq

instance (Num p, Show p, Show a) => Show (Categorical p a) where
    showsPrec p cat = showParen (p>10)
        ( showString "fromList "
        . showsPrec 11 (toList cat)
        )

instance (Num p, Read p, Read a) => Read (Categorical p a) where
  readsPrec p = readParen (p > 10) $ \str -> do
                  ("fromList", valStr) <- lex str
                  (vals,       rest)   <- readsPrec 11 valStr
                  return (fromList vals, rest)

instance (Fractional p, Ord p, Distribution Uniform p) => Distribution (Categorical p) a where
    rvarT (Categorical ds)
        | V.null ds = fail "categorical distribution over empty set cannot be sampled"
        | n == 1    = return (snd (V.head ds))
        | otherwise = do
            u <- uniformT 0 (fst (V.last ds))
            
            let -- by construction, p is monotone; (i < j) ==> (p i <= p j)
                p i = fst (ds V.! i)
                x i = snd (ds V.! i)
                
                --  findEvent
                -- ===========
                -- invariants: (i <= j), (u <= p j), ((i == 0) || (p i < u))
                --  (the last one means 'i' does not increase unless it bounds 'p' below 'u')
                -- variant: either i increases or j decreases.
                -- upon termination: ∀ k. if (k < j) then (p k < u) else (u <= p k)
                --  (that is, the chosen event 'x j' is the first one whose 
                --   associated cumulative probability 'p j' is greater than 
                --   or equal to 'u')
                findEvent i j
                    | j <= i    = x j
                    | u <= p m  = findEvent i m
                    | otherwise = findEvent (max m (i+1)) j
                    where
                        -- midpoint rounding down
                        -- (i < j) ==> (m < j)
                        m = (i + j) `div` 2
            
            return $! if u <= 0 then x 0 else findEvent 0 (n-1)
        where n = V.length ds


instance Functor (Categorical p) where
    fmap f (Categorical ds) = Categorical (V.map (second f) ds)

instance Foldable (Categorical p) where
    foldMap f (Categorical ds) = foldMap (f . snd) (V.toList ds)

instance Traversable (Categorical p) where
    traverse f (Categorical ds) = Categorical . V.fromList <$> traverse (\(p,e) -> (\e' -> (p,e')) <$> f e) (V.toList ds)
    sequenceA  (Categorical ds) = Categorical . V.fromList <$> traverse (\(p,e) -> (\e' -> (p,e')) <$>   e) (V.toList ds)

instance Fractional p => Monad (Categorical p) where
    return x = Categorical (V.singleton (1, x))
    
    -- I'm not entirely sure whether this is a valid form of failure; see next
    -- set of comments.
    fail _ = Categorical V.empty
    
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
    xs >>= f = {- normalizeCategoricalPs . -} fromList $ do
        (p, x) <- toList xs
        (q, y) <- toList (f x)
        
        return (p * q, y)

instance Fractional p => Applicative (Categorical p) where
    pure = return
    (<*>) = ap

-- |Like 'fmap', but for the probabilities of a categorical distribution.
mapCategoricalPs :: (Num p, Num q) => (p -> q) -> Categorical p e -> Categorical q e
mapCategoricalPs f = fromList . map (first f) . toList

-- |Adjust all the weights of a categorical distribution so that they 
-- sum to unity and remove all events whose probability is zero.
normalizeCategoricalPs :: (Fractional p, Eq p) => Categorical p e -> Categorical p e
normalizeCategoricalPs orig@(Categorical ds)
    | ps == 0   = Categorical V.empty
    | otherwise = runST $ do
        lastP       <- newSTRef 0
        nDups       <- newSTRef 0
        normalized  <- V.thaw ds
        
        let n           = V.length ds
            skip        = modifySTRef' nDups (1+)
            save i p x  = do
                d <- readSTRef nDups
                MV.write normalized (i-d) (p, x)
        
        sequence_
            [ do
                let (p,x) = ds V.! i
                p0 <- readSTRef lastP
                if p == p0
                    then skip
                    else do
                        save i (p * scale) x
                        writeSTRef lastP $! p
            | i <- [0..n-1]
            ]
        
        -- force last element to 1
        d <- readSTRef nDups
        let n' = n-d
        (_,lastX) <- MV.read normalized (n'-1)
        MV.write normalized (n'-1) (1,lastX)
        Categorical <$> V.unsafeFreeze (MV.unsafeSlice 0 n' normalized)
    where
        ps = totalWeight orig
        scale = recip ps

#if __GLASGOW_HASKELL__ < 706
-- |strict 'modifySTRef'
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' x f = do
    v <- readSTRef x
    let fv = f v
    fv `seq` writeSTRef x fv
#endif

-- |Simplify a categorical distribution by combining equivalent events (the new
-- event will have a probability equal to the sum of all the originals).
collectEvents :: (Ord e, Num p, Ord p) => Categorical p e -> Categorical p e
collectEvents = collectEventsBy compare ((sum *** head) . unzip)
        
-- |Simplify a categorical distribution by combining equivalent events (the new
-- event will have a weight equal to the sum of all the originals).
-- The comparator function is used to identify events to combine.  Once chosen,
-- the events and their weights are combined by the provided probability and
-- event aggregation function.
collectEventsBy :: Num p => (e -> e -> Ordering) -> ([(p,e)] -> (p,e))-> Categorical p e -> Categorical p e
collectEventsBy compareE combine = 
    fromList . map combine . groupEvents . sortEvents . toList
    where
        groupEvents = groupBy (\x y -> snd x `compareE` snd y == EQ)
        sortEvents  = sortBy (compareE `on` snd)
