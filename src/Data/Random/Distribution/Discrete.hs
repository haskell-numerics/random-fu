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

import Control.Monad
import Control.Applicative

import Data.List
import Data.Function

discrete :: Distribution (Discrete p) a => [(p,a)] -> RVar a
discrete ps = rvar (Discrete ps)

newtype Discrete p a = Discrete [(p, a)]
    deriving (Eq, Show)

instance (Num p, Ord p, Distribution Uniform p) => Distribution (Discrete p) a where
    rvar (Discrete []) = fail "discrete distribution over empty set cannot be sampled"
    rvar (Discrete ds) = do
        let (ps, xs) = unzip ds
            cs = scanl1 (+) ps
        
        when (any (<0) ps) $ fail "negative probability in discrete distribution"
        
        u <- uniform 0 (last cs)
        return $ head
            [ x
            | (c,x) <- zip cs xs
            , c >= u
            ]

instance Functor (Discrete p) where
    fmap f (Discrete ds) = Discrete [(p, f x) | (p, x) <- ds]

-- TODO - check out whether this is valid when not requiring normalization...
-- We want each subset of cases in fx derived from a given case 
-- in x to have the same total probability as the set in x from whence they came.
--
-- thus, w(f x) == w (x) is sufficient (although not necessary), where w() is
-- the weight.
instance (Fractional p, Ord p) => Monad (Discrete p) where
    return x = Discrete [(1, x)]
    (Discrete x) >>= f = Discrete $ do
        (p, x) <- x
        
        let Discrete fx = f x
        let qx = [ (q, x)
                 | (q, x) <- fx
                 , q > 0
                 ]
            qs = map fst qx     -- either (qx == []) or (sum qs > 0)
            scale = recip (sum qs)
        
        (q, x) <- qx
        -- now (qx /= []), because ((q,x) `elem` qx)
        -- therefore sum qs > 0, therefore 0 < scale < ∞.
        
        return (p * q * scale, x)

instance (Fractional p, Ord p) => Applicative (Discrete p) where
    pure = return
    (<*>) = ap

collectDiscreteEvents :: (Ord e, Num p, Ord p) => Discrete p e -> Discrete p e
collectDiscreteEvents (Discrete ds) = 
    Discrete . concatMap (uncurry combine . unzip) . groupEvents . sortEvents $ ds
    
    where
        groupEvents = groupBy ((==) `on` snd)
        sortEvents  = sortBy (compare `on` snd)
        
        -- don't combine negative weights with positive ones.
        -- don't error out ether - just leave them alone, it'll
        -- all barf when the distribution is sampled.
        combine ps (x:_) = case partition (> 0) (filter (/= 0) ps) of
            ([], []) ->                             []
            ([], ns) ->               (sum ns, x) : []
            (ps, []) -> (sum ps, x) :               []
            (ps, ns) -> (sum ps, x) : (sum ns, x) : []
        
        weight (p,x)
            | p < 0     = error "negative probability in discrete distribution"
            | otherwise = p
        event ((p,x):_) = x