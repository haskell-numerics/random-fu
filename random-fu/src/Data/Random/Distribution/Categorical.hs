{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts
  #-}

module Data.Random.Distribution.Categorical
    ( Categorical(..)
    , categorical, categoricalT
    , fromList, toList
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
import Control.Applicative
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

-- |Construct a 'Categorical' distribution from a list of weighted categories, 
-- where the weights do not necessarily sum to 1.
fromWeightedList :: Fractional p => [(p,a)] -> Categorical p a
fromWeightedList = normalizeCategoricalPs . fromList

-- |Construct a 'Categorical' distribution from a list of observed outcomes.
-- Equivalent events will be grouped and counted, and the probabilities of each
-- event in the returned distribution will be proportional to the number of 
-- occurrences of that event.
fromObservations :: (Fractional p, Ord a) => [a] -> Categorical p a
fromObservations = fromWeightedList . map (genericLength &&& head) . group . sort

-- |Categorical distribution; a list of events with corresponding probabilities.
-- The sum of the probabilities must be 1, and no event should have a zero 
-- or negative probability (at least, at time of sampling; very clever users
-- can do what they want with the numbers before sampling, just make sure 
-- that if you're one of those clever ones, you normalize before sampling).
newtype Categorical p a = Categorical (V.Vector (p, a))
    deriving Eq

instance (Num p, Show a) => Show (Categorical p a) where
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
            
            let p i = fst (ds V.! i)
                x i = snd (ds V.! i)
                
                -- find the smallest entry whose cumulative probability is
                -- greater than or equal to u
                -- invariant: p j >= u
                -- variant: at every step, either i increases or j decreases.
                findEvent i j
                    | i >= j    = x j
                    | p m >= u  = findEvent i m
                    | otherwise = findEvent (max m (i+1)) j
                    where
                        -- midpoint rounding down
                        m = (i + j) `div` 2
            
            return (findEvent 0 (n-1))
        where n = V.length ds


instance Functor (Categorical p) where
    fmap f (Categorical ds) = Categorical (V.map (second f) ds)

instance Foldable (Categorical p) where
    foldMap f (Categorical ds) = foldMap (f . snd) (V.toList ds)

instance Traversable (Categorical p) where
    traverse f (Categorical ds) = Categorical . V.fromList <$> traverse (\(p,e) -> (\e' -> (p,e')) <$> f e) (V.toList ds)
    sequenceA  (Categorical ds) = Categorical . V.fromList <$> traverse (\(p,e) -> (\e' -> (p,e')) <$>   e) (V.toList ds)

instance Num p => Monad (Categorical p) where
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
mapCategoricalPs :: (p -> q) -> Categorical p e -> Categorical q e
mapCategoricalPs f (Categorical ds) = Categorical (V.map (first f) ds)

-- |Adjust all the weights of a categorical distribution so that they 
-- sum to unity.
normalizeCategoricalPs :: (Fractional p) => Categorical p e -> Categorical p e
normalizeCategoricalPs orig@(Categorical ds) = 
    if V.null ds
        then orig
        else runST $ do
            let n = V.length ds
            lastP       <- newSTRef 0
            dups        <- newSTRef 0
            normalized  <- V.thaw ds
            
            let skip = modifySTRef' dups (1+)
                save i p x = do
                    d <- readSTRef dups
                    MV.write normalized (i-d) (p, x)
            
            sequence_
                [ do
                    let (p,x) = ds V.! i
                    p0 <- readSTRef lastP
                    if p == p0
                        then skip
                        else do
                            save i (p * scale) x
                            writeSTRef lastP p
                | i <- [0..n-1]
                ]
            
            -- force last element to 1
            d <- readSTRef dups
            MV.write normalized (n-d-1) (1,lastX)
            Categorical <$> V.unsafeFreeze (MV.unsafeSlice 0 (n-d) normalized)
    where
        (ps, lastX) = V.last ds
        scale = recip ps

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' x f = do
    v <- readSTRef x
    let fv = f v
    fv `seq` writeSTRef x fv

-- |Simplify a categorical distribution by combining equivalent categories (the new
-- category will have a probability equal to the sum of all the originals).
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
