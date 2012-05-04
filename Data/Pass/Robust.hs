module Data.Pass.Robust
  ( Robust(..)
  , median
  , iqm
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
  robust :: (Fractional a, Ord a) => L a a -> l a a

  winsorized :: Rational -> L Double Double -> l Double Double
  winsorized p f = robust $ Winsorized p f

  trimmed :: Rational -> L Double Double -> l Double Double
  trimmed p f = robust $ Trimmed p f

  jackknifed :: L Double Double -> l Double Double
  jackknifed f = robust $ Jackknifed f

  lscale :: l Double Double
  lscale = robust LScale

  quantile :: Robust l => Rational -> l Double Double
  quantile p = robust $ QuantileBy R2 p

  midhinge :: l Double Double
  midhinge = robust $ 0.5 :* (q1 :+ q3)

  -- | Tukey's trimean
  trimean :: l Double Double
  trimean = robust $ 0.25 :* (q1 :+ 2 :* q2 :+ q3)

  -- | interquartile range
  iqr :: l Double Double
  iqr = robust $ ((-1) :* q1) :+ q3

-- | interquartile mean
iqm :: Robust l => l Double Double
iqm = trimmed 0.25 LMean

median :: Robust l => l Double Double
median = quantile 0.5

tercile :: Robust l => Rational -> l Double Double
tercile n = quantile (n/3)

-- | terciles 1 and 2
t1, t2 :: Robust l => l Double Double
t1 = tercile 1
t2 = tercile 2

quartile :: Robust l => Rational -> l Double Double
quartile n = quantile (n/4)

-- | quantiles, with breakdown points 25%, 50%, and 25% respectively
q1, q2, q3 :: Robust l => l Double Double
q1 = quantile 0.25
q2 = median
q3 = quantile 0.75

quintile :: Robust l => Rational -> l Double Double
quintile n = quantile (n/5)

-- | quintiles 1 through 4
qu1, qu2, qu3, qu4 :: Robust l => l Double Double
qu1 = quintile 1
qu2 = quintile 2
qu3 = quintile 3
qu4 = quintile 4

percentile :: Robust l => Rational -> l Double Double
percentile n = quantile (n/100)

permille :: Robust l => Rational -> l Double Double
permille n = quantile (n/1000)

instance Robust L where
  robust = id

instance Robust l => Robust (Fun l) where
  robust = Fun . robust

instance Robust (Pass k) where
  robust l = L id l Nil
  midhinge = (q1 + q3) / 2
  trimean  = (q1 + 2 * q2 + q3) / 4
  iqr      = q3 - q1

instance Robust (Calc k) where
  robust l = Stop () `Step` const (robust l)
  midhinge = Stop () `Step` const midhinge
  trimean  = Stop () `Step` const trimean
  iqr      = Stop () `Step` const iqr
