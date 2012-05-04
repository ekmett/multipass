{-# LANGUAGE GADTs, DeriveDataTypeable, PatternGuards, Rank2Types #-}

module Data.Pass.L
  ( L(..)
  , getL
  , callL
  , ordL
  , eqL
  , breakdown
  , (@#)
  ) where

import Control.Monad (liftM, liftM2)
import Data.Typeable
import Data.Hashable
import Data.Pass.Named
import Data.IntMap (IntMap)
import Data.Key (foldrWithKey)
import Data.List (sort)
import qualified Data.IntMap as IM
import qualified Data.Foldable as Foldable
import Data.Pass.L.Estimator
import Data.Pass.Eval
import Data.Pass.Eval.Naive
import Data.Pass.L.By
import Data.Pass.Util (clamp)
import Data.Binary

-- | An L-Estimator represents a linear combination of order statistics
data L a b where
  LTotal       :: (Num a, Ord a) => L a a
  LMean        :: (Fractional a, Ord a) => L a a
  LScale       :: (Fractional a, Ord a) => L a a
  NthLargest   :: (Num a, Ord a) => Int -> L a a
  NthSmallest  :: (Num a, Ord a) => Int -> L a a
  QuantileBy   :: (Fractional a, Ord a) => Estimator -> Rational -> L a a
  Winsorized   :: (Fractional b, Ord b) => Rational -> L a b -> L a b
  Trimmed      :: (Fractional b, Ord b) => Rational -> L a b -> L a b
  Jackknifed   :: (Fractional b, Ord b) => L a b -> L a b
  (:*)         :: Fractional b => Rational -> L a b -> L a b
  (:+)         :: Num b => L a b -> L a b -> L a b
  deriving Typeable

instance By L where
  QuantileBy _ q `by` r = QuantileBy r q
  Trimmed p f    `by` r = Trimmed p (by f r)
  Winsorized p f `by` r = Winsorized p (by f r)
  Jackknifed f   `by` r = Jackknifed (by f r)
  (n :* f)       `by` r = n :* by f r
  (f :+ g)       `by` r = by f r :+ by g r
  f              `by` _ = f

infixl 7 :*
infixl 6 :+

instance Named L where
  showsFun _ LTotal           = showString "LTotal"
  showsFun _ LMean            = showString "LMean"
  showsFun _ LScale           = showString "LScale"
  showsFun d (QuantileBy e q) = showParen (d > 10) $ showString "QuantileBy " . showsPrec 10 e . showChar ' ' . showsPrec 10 q
  showsFun d (Winsorized p f) = showParen (d > 10) $ showString "Winsorized " . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Trimmed p f)    = showParen (d > 10) $ showString "Trimmed "    . showsPrec 10 p . showChar ' ' . showsFun 10 f
  showsFun d (Jackknifed f)   = showParen (d > 10) $ showString "Jackknifed "                                 . showsFun 10 f
  showsFun d (NthLargest n)   = showParen (d > 10) $ showString "NthLargest "  . showsPrec 10 n
  showsFun d (NthSmallest n)  = showParen (d > 10) $ showString "NthSmallest " . showsPrec 10 n
  showsFun d (x :* y)         = showParen (d > 7) $ showsPrec 8 x . showString " :* " . showsPrec 7 y
  showsFun d (x :+ y)         = showParen (d > 6) $ showsPrec 7 x . showString " :+ " . showsPrec 6 y

  hashFunWithSalt n LTotal           = 0 `hashWithSalt` n
  hashFunWithSalt n LMean            = 1 `hashWithSalt` n
  hashFunWithSalt n LScale           = 2 `hashWithSalt` n
  hashFunWithSalt n (QuantileBy e q) = 4 `hashWithSalt` n  `hashWithSalt` e `hashWithSalt` q
  hashFunWithSalt n (Winsorized p f) = 5 `hashWithSalt` n  `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Trimmed p f)    = 6 `hashWithSalt` n  `hashWithSalt` p `hashFunWithSalt` f
  hashFunWithSalt n (Jackknifed f)   = 7 `hashWithSalt` n                  `hashFunWithSalt` f
  hashFunWithSalt n (NthLargest m)   = 8 `hashWithSalt` n  `hashWithSalt` m
  hashFunWithSalt n (NthSmallest m)  = 9 `hashWithSalt` n  `hashWithSalt` m
  hashFunWithSalt n (x :* y)         = 10 `hashWithSalt` n `hashWithSalt` x `hashFunWithSalt` y
  hashFunWithSalt n (x :+ y)         = 11 `hashWithSalt` n `hashFunWithSalt` x `hashFunWithSalt` y

  equalFun LTotal LTotal = True
  equalFun LMean  LMean  = True
  equalFun LScale LScale = True
  equalFun (QuantileBy e p) (QuantileBy f q) = e == f && p == q
  equalFun (Winsorized p f) (Winsorized q g) = p == q && equalFun f g
  equalFun (Trimmed p f) (Trimmed q g)       = p == q && equalFun f g
  equalFun (Jackknifed f) (Jackknifed g)     = equalFun f g
  equalFun (NthLargest n)  (NthLargest m)    = n == m
  equalFun (NthSmallest n) (NthSmallest m)   = n == m
  equalFun (a :+ b) (c :+ d)                 = equalFun a c && equalFun b d
  equalFun (a :* b) (c :* d)                 = typeOf a == typeOf c && cast a == Just c && equalFun b d
  equalFun _ _ = False

  putFun LTotal           = put (0 :: Word8)
  putFun LMean            = put (1 :: Word8)
  putFun LScale           = put (2 :: Word8)
  putFun (QuantileBy e r) = put (4 :: Word8) >> put e >> put r
  putFun (Winsorized p f) = put (5 :: Word8) >> put p >> putFun f
  putFun (Trimmed p f)    = put (6 :: Word8) >> put p >> putFun f
  putFun (Jackknifed f)   = put (7 :: Word8) >> putFun f
  putFun (NthLargest n)   = put (8 :: Word8) >> put n
  putFun (NthSmallest n)  = put (9 :: Word8) >> put n
  putFun (x :* y)         = put (10 :: Word8) >> put x >> putFun y
  putFun (x :+ y)         = put (11 :: Word8) >> putFun x >> putFun y

getL :: (Fractional a, Ord a) => Get (L a a)
getL = do
  i <- get :: Get Word8
  case i of
    0 -> return LTotal
    1 -> return LMean
    2 -> return LScale
    4 -> liftM2 QuantileBy get get
    5 -> liftM2 Winsorized get getL
    6 -> liftM2 Trimmed get getL
    7 -> liftM Jackknifed getL
    8 -> liftM NthLargest get
    9 -> liftM NthSmallest get
    10 -> liftM2 (:*) get getL
    11 -> liftM2 (:+) getL getL
    _  -> error "getL: Unknown L-estimator"

{-
instance (Fractional a, Ord a) => Binary (L a a) where
  put = putFun
  get = getL
-}

instance Naive L where
  naive = (@@)

instance Show (L a b) where
  showsPrec = showsFun

instance Hashable (L a b) where
  hashWithSalt = hashFunWithSalt

instance Eq (L a b) where
  (==) = equalFun

-- | A common measure of how robust an L estimator is in the presence of outliers.
breakdown :: (Num b, Eq b) => L a b -> Int
breakdown f
  | IM.null m = 50
  | otherwise = fst (IM.findMin m) `min` (100 - fst (IM.findMax m))
  where m = IM.filter (/= 0) $ callL f 101

infixl 0 @#

-- | @f \@# n@ Return a list of the coefficients that would be used by an L-Estimator for an input of length @n@
(@#) :: Num a => L a a -> Int -> [a]
f @# n = [ IM.findWithDefault 0 k fn | k <- [0..n-1] ]
  where fn = callL f n

callL :: L a b -> Int -> IntMap b
callL LTotal n = IM.fromList [ (i,1) | i <- [0..n-1]]
callL LMean n = IM.fromList [ (i, oon) | i <- [0..n-1]]
  where oon = recip (fromIntegral n)
callL LScale n = IM.fromList [ (i - 1, scale * (2 * fromIntegral i - 1 - r)) | i <- [1..n]]
  where r = fromIntegral n
        scale = 1 / (r *(r-1))
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
callL (s :* y) n = fmap (r *) (callL y n) where r = fromRational s

instance Eval L where
  m @@ as = ordL m $ foldrWithKey step 0 $ sort $ eqL m xs where
    xs = Foldable.toList as
    n = length xs
    coefs = callL m n
    step = ordL m $ \g v x -> IM.findWithDefault 0 g coefs * v + x

eqL :: L a b -> p a -> p b
eqL LTotal a = a
eqL LMean a = a
eqL LScale a = a
eqL (NthLargest _) a = a
eqL (NthSmallest _) a = a
eqL (QuantileBy _ _) a = a
eqL (Winsorized _ x) a = eqL x a
eqL (Jackknifed x) a = eqL x a
eqL (Trimmed _ x) a = eqL x a
eqL (x :+ _) a = eqL x a
eqL (_ :* x) a = eqL x a

ordL :: L a b -> ((Ord b, Num b) => r) -> r
ordL LTotal a = a
ordL LMean a = a
ordL LScale a = a
ordL (NthLargest _) a = a
ordL (NthSmallest _) a = a
ordL (QuantileBy _ _) a = a
ordL (Winsorized _ x) a = ordL x a
ordL (Trimmed _ x) a = ordL x a
ordL (Jackknifed x) a = ordL x a
ordL (x :+ _) a = ordL x a
ordL (_ :* x) a = ordL x a
