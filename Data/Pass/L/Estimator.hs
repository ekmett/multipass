{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.L.Estimator
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.L.Estimator
  ( Estimator(..)
  , Estimate(..)
  , estimateBy
  ) where

import Data.Ratio
import Data.Binary
import Data.Data
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Pass.Util (clamp)
import Data.Hashable
import Data.Pass.Util.Beta (Beta(Beta))
import qualified Data.Pass.Util.Beta as Beta

-- | Techniques used to smooth the nearest values when calculating quantile functions. R2 is used by default, and the numbering convention follows the 
-- use in the R programming language, as far as it goes.
data Estimator
  = R1  -- ^ Inverse of the empirical distribution function
  | R2  -- ^ .. with averaging at discontinuities (default)
  | R3  -- ^ The observation numbered closest to Np. NB: does not yield a proper median
  | R4  -- ^ Linear interpolation of the empirical distribution function. NB: does not yield a proper median.
  | R5  -- ^ .. with knots midway through the steps as used in hydrology. This is the simplest continuous estimator that yields a correct median
  | R6  -- ^ Linear interpolation of the expectations of the order statistics for the uniform distribution on [0,1]
  | R7  -- ^ Linear interpolation of the modes for the order statistics for the uniform distribution on [0,1]
  | R8  -- ^ Linear interpolation of the approximate medans for order statistics.
  | R9  -- ^ The resulting quantile estimates are approximately unbiased for the expected order statistics if x is normally distributed.
  | R10 -- ^ When rounding h, this yields the order statistic with the least expected square deviation relative to p.
  | HD  -- ^ The Harrell-Davis quantile estimator based on bootstrapped order statistics
  deriving (Eq,Ord,Enum,Bounded,Data,Typeable,Show,Read)

instance Binary Estimator where
  put e = put (fromIntegral (fromEnum e) :: Word8)
  get = do
    i <- get :: Get Word8
    return $ toEnum (fromIntegral i)

instance Hashable Estimator where
  hashWithSalt n e = n `hashWithSalt` fromEnum e

data Estimate r = Estimate {-# UNPACK #-} !Rational (IntMap r)
  deriving Show

continuousEstimator ::
  Fractional r =>
  (Rational -> (Rational, Rational)) ->
  (Rational -> Rational -> Rational) ->
  Rational -> Int -> Estimate r
continuousEstimator bds f p n = Estimate h $
  if p < lo then IM.singleton 0 1
  else if p >= hi then IM.singleton (n - 1) 1
  else case properFraction h of
    (w,frac) | frac' <- fromRational frac -> IM.fromList [(w - 1, frac'), (w, 1 - frac')]
  where
    r = fromIntegral n
    h = f p r
    (lo, hi) = bds r

estimateBy :: Fractional r => Estimator -> Rational -> Int -> Estimate r
estimateBy HD = \q n -> Estimate (1%2) $ let
    n' = fromIntegral n
    np1 = n' + 1
    q' = fromRational q
    d = Beta (q'*np1) (np1*(1-q'))
  in if q == 0 then IM.singleton 0 1
  else if q == 1 then IM.singleton (n - 1) 1
  else IM.fromListWith (+)
    [ (i, realToFrac $ Beta.cumulative d ((fromIntegral i + 1) / n') - Beta.cumulative d (fromIntegral i / n'))
    | i <- [0 .. n-1]
    ]
estimateBy R1  = \p n -> let np = fromIntegral n * p in Estimate (np + 1%2) $ IM.singleton (clamp n (ceiling np - 1)) 1
estimateBy R2  = \p n -> let np = fromIntegral n * p in Estimate (np + 1%2) $
  if p == 0      then IM.singleton 0       1
  else if p == 1 then IM.singleton (n - 1) 1
  else IM.fromListWith (+) [(clamp n (ceiling np - 1), 0.5), (clamp n (floor np), 0.5)]
estimateBy R3  = \p n -> let np = fromIntegral n * p in Estimate np $ IM.singleton (clamp n (round np - 1)) 1
estimateBy R4  = continuousEstimator (\n -> (recip n, 1)) (*)
estimateBy R5  = continuousEstimator (\n -> let tn = 2 * n in (recip tn, (tn - 1) / tn)) $ \p n -> p*n + 0.5
estimateBy R6  = continuousEstimator (\n -> (recip (n + 1), n / (n + 1))) $ \p n -> p*(n+1)
estimateBy R7  = continuousEstimator (\_ -> (0, 1)) $ \p n -> p*(n-1) + 1
estimateBy R8  = continuousEstimator (\n -> (2/3 / (n + 1/3), (n - 1/3)/(n + 1/3))) $ \p n -> p*(n + 1/3) + 1/3
estimateBy R9  = continuousEstimator (\n -> (0.625 / (n + 0.25), (n - 0.375)/(n + 0.25))) $ \p n -> p*(n + 0.25) + 0.375
estimateBy R10 = continuousEstimator (\n -> (1.5 / (n + 2), (n + 0.5)/(n + 2))) $ \p n -> p*(n + 2) - 0.5
