module Data.Pass.Robust
  ( Robust(..)
  , quantile
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
import Data.Pass.Estimator

-- | embedding for L-estimators
class Robust l where
  robust :: (Fractional a, Ord a) => L a a -> l a a

  winsorized :: Rational -> L Double Double -> l Double Double
  winsorized p f = robust $ Winsorized p f

  trimmed :: Rational -> L Double Double -> l Double Double
  trimmed p f = robust $ Trimmed p f

  jackknifed :: L Double Double -> l Double Double
  jackknifed f = robust $ Jackknifed f

  median :: l Double Double
  median = robust Median

  lscale :: l Double Double
  lscale = robust LScale

  quantileBy :: Estimator -> Rational -> l Double Double
  quantileBy e q = robust $ QuantileBy e q

quantile :: Robust l => Rational -> l Double Double
quantile = quantileBy R2

tercile :: Robust l => Rational -> l Double Double
tercile n = quantile (n/3)

-- | terciles 1 and 2
--
-- > breakdown t1 = breakdown t2 = 33%
t1, t2 :: Robust l => l Double Double
t1 = quantile (1/3)
t2 = quantile (2/3)

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

instance Robust (Calc k) where
  robust l = Stop () `Step` \_ -> robust l
