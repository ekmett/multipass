module Data.Pass.Robust
  ( Robust(..)
  , median
  , iqm
  , idm
  , tercile, t1, t2
  , quartile, q1, q2, q3
  , quintile, qu1, qu2, qu3, qu4
  , percentile
  , permille
  ) where

import Data.Pass.Type
import Data.Pass.Calc
import Data.Pass.Fun
import Data.Pass.Thrist
import Data.Pass.L
import Data.Pass.L.Estimator

-- | embedding for L-estimators
class Robust l where
  robust :: L a b -> l a b

  winsorized :: (Fractional b, Ord b) => Rational -> L a b -> l a b
  winsorized p f = robust $ Winsorized p f

  trimmed :: (Fractional b, Ord b) => Rational -> L a b -> l a b
  trimmed p f = robust $ Trimmed p f

  jackknifed :: (Fractional b, Ord b) => L a b -> l a b
  jackknifed f = robust $ Jackknifed f

  lscale :: (Fractional a, Ord a) => l a a
  lscale = robust LScale

  quantile :: (Fractional a, Ord a) => Rational -> l a a
  quantile p = robust $ QuantileBy R2 p

  midhinge :: (Fractional a, Ord a) => l a a
  midhinge = robust $ 0.5 :* (q1 :+ q3)

  -- | Tukey's trimean
  trimean :: (Fractional a, Ord a) => l a a
  trimean = robust $ 0.25 :* (q1 :+ 2 :* q2 :+ q3)

  -- | interquartile range
  iqr :: (Fractional a, Ord a) => l a a
  iqr = robust $ ((-1) :* q1) :+ q3

  idr :: (Fractional a, Ord a) => l a a
  idr = robust $ ((-1) :* quantile 0.1) :+ quantile 0.9

-- | interquartile mean
iqm :: (Robust l, Fractional a, Ord a) => l a a
iqm = trimmed 0.25 LMean

idm :: (Robust l, Fractional a, Ord a) => l a a
idm = trimmed 0.1 LMean

median :: (Robust l, Fractional a, Ord a) => l a a
median = quantile 0.5

tercile :: (Robust l, Fractional a, Ord a) => Rational -> l a a
tercile n = quantile (n/3)

-- | terciles 1 and 2
t1, t2 :: (Robust l, Fractional a, Ord a) => l a a
t1 = tercile 1
t2 = tercile 2

quartile :: (Robust l, Fractional a, Ord a) => Rational -> l a a
quartile n = quantile (n/4)

-- | quantiles, with breakdown points 25%, 50%, and 25% respectively
q1, q2, q3 :: (Robust l, Fractional a, Ord a) => l a a
q1 = quantile 0.25
q2 = median
q3 = quantile 0.75

quintile :: (Robust l, Fractional a, Ord a) => Rational -> l a a
quintile n = quantile (n/5)

-- | quintiles 1 through 4
qu1, qu2, qu3, qu4 :: (Robust l, Fractional a, Ord a) => l a a
qu1 = quintile 1
qu2 = quintile 2
qu3 = quintile 3
qu4 = quintile 4

percentile :: (Robust l, Fractional a, Ord a) => Rational -> l a a
percentile n = quantile (n/100)

permille :: (Robust l, Fractional a, Ord a) => Rational -> l a a
permille n = quantile (n/1000)

instance Robust L where
  robust = id

instance Robust l => Robust (Fun l) where
  robust = Fun . robust

newtype Flip f a b = Flip { flop :: f b a }

instance Robust (Pass k) where
  robust l = L id (flop (eqL l (Flip l))) (eqL l Nil)
  midhinge = (q1 + q3) / 2
  trimean  = (q1 + 2 * q2 + q3) / 4
  iqr      = q3 - q1

instance Robust (Calc k) where
  robust l = Stop () `Step` const (robust l)
  midhinge = Stop () `Step` const midhinge
  trimean  = Stop () `Step` const trimean
  iqr      = Stop () `Step` const iqr
