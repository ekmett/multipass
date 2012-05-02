{-# LANGUAGE GADTs, DeriveDataTypeable, ExtendedDefaultRules, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Pass.L
  ( L(..)
  , callL
  ) where

import Data.Typeable
import Data.Hashable
import Data.Pass.Named
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data L a b where
  LTotal     :: L Double Double
  LMean      :: L Double Double
  LScale     :: L Double Double
  Median     :: L Double Double
  Winsorized :: Rational -> L a Double -> L a Double
  Trimmed    :: Rational -> L a Double -> L a Double
  Jackknifed :: L a Double -> L a Double
  (:*)       :: Double -> L a Double -> L a Double
  (:+)       :: L a Double -> L a Double -> L a Double
  deriving Typeable

infixl 7 :*
infixl 6 :+

instance Named L where
  showsFun _ LTotal           = showString "LTotal"
  showsFun _ LMean            = showString "LMean"
  showsFun _ LScale           = showString "LScale"
  showsFun _ Median           = showString "Median"
  showsFun d (Winsorized p f) = showParen (d > 10) $ showString "Winsorized " . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Trimmed p f)    = showParen (d > 10) $ showString "Trimmed"     . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Jackknifed f)   = showParen (d > 10) $ showString "Jackknifed "                                 . showsFun 10 f
  showsFun d (x :* y)         = showParen (d > 7) $ showsPrec 8 x . showString " :* " . showsPrec 7 y
  showsFun d (x :+ y)         = showParen (d > 6) $ showsPrec 7 x . showString " :+ " . showsPrec 6 y
  hashFunWithSalt n LTotal           = n `hashWithSalt` 0
  hashFunWithSalt n LMean            = n `hashWithSalt` 1
  hashFunWithSalt n LScale           = n `hashWithSalt` 2
  hashFunWithSalt n Median           = n `hashWithSalt` 3
  hashFunWithSalt n (Winsorized p f) = n `hashWithSalt` 4 `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Trimmed p f)    = n `hashWithSalt` 5 `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Jackknifed f)   = n `hashWithSalt` 6                  `hashFunWithSalt` f
  hashFunWithSalt n (x :* y) = n `hashWithSalt` x `hashFunWithSalt` y
  hashFunWithSalt n (x :+ y) = n `hashFunWithSalt` x `hashFunWithSalt` y
  equalFun LTotal LTotal = True
  equalFun LMean LMean = True
  equalFun LScale LScale = True
  equalFun Median Median = True
  equalFun (Winsorized p f) (Winsorized q g) = p == q && equalFun f g
  equalFun (Trimmed p f) (Trimmed q g) = p == q && equalFun f g
  equalFun (Jackknifed f) (Jackknifed g) = equalFun f g
  equalFun (a :+ b) (c :+ d) = equalFun a c && equalFun b d
  equalFun (a :* b) (c :* d) = a == c && equalFun b d
  equalFun _ _ = False

instance Show (L a b) where
  showsPrec = showsFun

instance Hashable (L a b) where
  hashWithSalt = hashFunWithSalt

instance Eq (L a b) where
  (==) = equalFun

callL :: L a a -> Int -> IntMap a
callL (x :+ y) n = IM.unionWith (+) (callL x n) (callL y n)
callL (s :* y) n = fmap (s*) (callL y n)
callL Median n = case quotRem n 2 of
  (k, 0) -> IM.fromDistinctAscList [(k-1,0.5),(k,0.5)]
  (k, _) -> IM.singleton k 1
callL LMean n = IM.fromList [ (i, oon) | i <- [0..n-1]]
  where oon = recip (fromIntegral n)
callL LScale n = IM.fromList [ (i - 1, scale * (2 * fromIntegral i - 1 - r)) | i <- [1..n]]
  where r = fromIntegral n
        scale = 1 / (r *(r-1))
callL LTotal n = IM.fromList [ (i,1) | i <- [0..n-1]]
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
