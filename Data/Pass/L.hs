{-# LANGUAGE GADTs, DeriveDataTypeable, ExtendedDefaultRules, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Pass.L
  ( L(..)
  , callL
  , breakdown
  , (@#)
  ) where

import Data.Typeable
import Data.Hashable
import Data.Pass.Named
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Pass.L.Estimator
import Data.Pass.Util (clamp)
import Data.Pass.Util.Beta (Beta(Beta))
import qualified Data.Pass.Util.Beta as Beta

(@#) :: Num a => L a a -> Int -> [a]
f @# n = [ IM.findWithDefault 0 k fn | k <- [0..n-1] ]
  where fn = callL f n

data L a b where
  LTotal       :: L Double Double
  LMean        :: L Double Double
  LScale       :: L Double Double
  Median       :: L Double Double
  QuantileBy   :: Estimator -> Rational -> L Double Double
  Winsorized   :: Rational -> L a Double -> L a Double
  Trimmed      :: Rational -> L a Double -> L a Double
  Jackknifed   :: L a Double -> L a Double
  NthLargest   :: Int -> L Double Double
  NthSmallest  :: Int -> L Double Double
  (:*)         :: Double -> L a Double -> L a Double
  (:+)         :: L a Double -> L a Double -> L a Double
  HarrellDavis :: Rational -> L Double Double
  deriving Typeable

infixl 7 :*
infixl 6 :+

instance Named L where
  showsFun _ LTotal           = showString "LTotal"
  showsFun _ LMean            = showString "LMean"
  showsFun _ LScale           = showString "LScale"
  showsFun _ Median           = showString "Median"
  showsFun d (QuantileBy e q) = showParen (d > 10) $ showString "QuantileBy " . showsPrec 10 e . showChar ' ' . showsPrec 10 q
  showsFun d (Winsorized p f) = showParen (d > 10) $ showString "Winsorized " . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Trimmed p f)    = showParen (d > 10) $ showString "Trimmed "    . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Jackknifed f)   = showParen (d > 10) $ showString "Jackknifed "                                 . showsFun 10 f
  showsFun d (NthLargest n)   = showParen (d > 10) $ showString "NthLargest "  . showsPrec 10 n
  showsFun d (NthSmallest n)  = showParen (d > 10) $ showString "NthSmallest " . showsPrec 10 n
  showsFun d (x :* y)         = showParen (d > 7) $ showsPrec 8 x . showString " :* " . showsPrec 7 y
  showsFun d (x :+ y)         = showParen (d > 6) $ showsPrec 7 x . showString " :+ " . showsPrec 6 y
  showsFun d (HarrellDavis p) = showParen (d > 10) $ showString "HarrellDavis " . showsPrec 10 p

  hashFunWithSalt n LTotal           = n `hashWithSalt` 0
  hashFunWithSalt n LMean            = n `hashWithSalt` 1
  hashFunWithSalt n LScale           = n `hashWithSalt` 2
  hashFunWithSalt n Median           = n `hashWithSalt` 3
  hashFunWithSalt n (QuantileBy e q) = n `hashWithSalt` 4  `hashWithSalt` e `hashWithSalt` q
  hashFunWithSalt n (Winsorized p f) = n `hashWithSalt` 5  `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Trimmed p f)    = n `hashWithSalt` 6  `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Jackknifed f)   = n `hashWithSalt` 7                  `hashFunWithSalt` f
  hashFunWithSalt n (NthLargest m)   = n `hashWithSalt` 8  `hashWithSalt` m
  hashFunWithSalt n (NthSmallest m)  = n `hashWithSalt` 9  `hashWithSalt` m
  hashFunWithSalt n (x :* y)         = n `hashWithSalt` 10 `hashWithSalt` x `hashFunWithSalt` y
  hashFunWithSalt n (x :+ y)         = n `hashWithSalt` 11 `hashFunWithSalt` x `hashFunWithSalt` y
  hashFunWithSalt n (HarrellDavis p) = n `hashWithSalt` 12 `hashWithSalt` p

  equalFun LTotal LTotal = True
  equalFun LMean  LMean  = True
  equalFun LScale LScale = True
  equalFun Median Median = True
  equalFun (QuantileBy e p) (QuantileBy f q) = e == f && p == q
  equalFun (Winsorized p f) (Winsorized q g) = p == q && equalFun f g
  equalFun (Trimmed p f) (Trimmed q g)       = p == q && equalFun f g
  equalFun (Jackknifed f) (Jackknifed g)     = equalFun f g
  equalFun (NthLargest n)  (NthLargest m)    = n == m
  equalFun (NthSmallest n) (NthSmallest m)   = n == m
  equalFun (a :+ b) (c :+ d)                 = equalFun a c && equalFun b d
  equalFun (a :* b) (c :* d)                 = a == c && equalFun b d
  equalFun (HarrellDavis p) (HarrellDavis q) = p == q
  equalFun _ _ = False

instance Show (L a b) where
  showsPrec = showsFun

instance Hashable (L a b) where
  hashWithSalt = hashFunWithSalt

instance Eq (L a b) where
  (==) = equalFun

callL :: L a a -> Int -> IntMap a
callL LTotal n = IM.fromList [ (i,1) | i <- [0..n-1]]
callL LMean n = IM.fromList [ (i, oon) | i <- [0..n-1]]
  where oon = recip (fromIntegral n)
callL LScale n = IM.fromList [ (i - 1, scale * (2 * fromIntegral i - 1 - r)) | i <- [1..n]]
  where r = fromIntegral n
        scale = 1 / (r *(r-1))
callL Median n = case quotRem n 2 of
  (k, 0) -> IM.fromDistinctAscList [(k-1,0.5),(k,0.5)]
  (k, _) -> IM.singleton k 1
callL (QuantileBy f p) n = case estimateBy f p n of
  Estimate h qp -> case properFraction h of
    (w, 0) -> IM.singleton (clamp n (w - 1)) 1
    _      -> qp
callL (Winsorized p g) n = case properFraction (fromIntegral n * p) of
  (w, 0) -> IM.fromAscListWith (+) [ (w `max` min (n - 1 - w) k, v) | (k,v) <- IM.toAscList (callL g n) ]
  (w, f) | w' <- w + 1 -> IM.fromListWith (+) $ do
     (k,v) <- IM.toList (callL g n)
     [ (w  `max` min (n - 1 - w ) k, v * fromRational (1 - f)),
       (w' `max` min (n - 1 - w') k, v * fromRational f)]
callL (Trimmed p g) n = case properFraction (fromIntegral n * p) of
  (w, 0)               -> IM.fromDistinctAscList [ (k + w, v) | (k, v) <- IM.toAscList $ callL g (n - w*2)]
  (w, f) | w' <- w + 1 -> IM.fromListWith (+) $ [ (k + w, fromRational (1 - f) * v) | (k,v) <- IM.toList $ callL g (n - w*2)] ++
                                                [ (k + w', fromRational f  * v)     | (k,v) <- IM.toList $ callL g (n - w'*2)]
callL (Jackknifed g) n = IM.fromAscListWith (+) $ do
  let n' = fromIntegral n
  (k, v) <- IM.toAscList $ callL g (n - 1)
  let k' = fromIntegral k + 1
  [(k, (n' - k') * v / n'), (k + 1, k' * v / n')]
callL (NthLargest m) n  = IM.singleton (clamp n (n - m - 1)) 1
callL (NthSmallest m) n = IM.singleton (clamp n m) 1
callL (x :+ y) n = IM.unionWith (+) (callL x n) (callL y n)
callL (s :* y) n = fmap (s*) (callL y n)
callL (HarrellDavis q) n
  | q == 0 = IM.singleton 0 1
  | q == 1 = IM.singleton (n - 1) 1
  | otherwise = IM.fromAscList
    [ (i, realToFrac $ Beta.cumulative d ((fromIntegral i + 1) / n') - Beta.cumulative d (fromIntegral i / n'))
    | i <- [0 .. n - 1]
    ]
  where
    n' = fromIntegral n
    np1 = n' + 1
    q' = fromRational q
    d = Beta (q'*np1) (np1*(1-q'))

breakdown :: (Num a, Eq a) => L a a -> Int
breakdown f
  | IM.null m = 50
  | otherwise = fst (IM.findMin m) `min` (100 - fst (IM.findMax m))
  where m = IM.filter (/= 0) $ callL f 101
