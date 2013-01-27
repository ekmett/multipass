-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Util.Beta
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Util.Beta
  ( Beta(..)
  , cumulative
  , density
  , quantile
  ) where

import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants (m_NaN)

data Beta = Beta {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq,Read,Show)

cumulative :: Beta -> Double -> Double
cumulative (Beta a b) x
  | x <= 0 = 0
  | otherwise = incompleteBeta a b x
{-# INLINE cumulative #-}

-- invert a monotone function
invertMono :: (Double -> Double) -> Double -> Double -> Double -> Double
invertMono f l0 h0 b = go l0 h0 where
  go l h
    | h - l < epsilon = m
    | otherwise = case compare (f m) b of
      LT -> go m h
      EQ -> m
      GT -> go l m
    where m = l + (h-l)/2
          epsilon = 1e-12
{-# INLINE invertMono #-}

density :: Beta -> Double -> Double
density (Beta a b) x
  | a <= 0 || b <= 0 = m_NaN
  | x <= 0 = 0
  | x >= 1 = 0
  | otherwise = exp $ (a-1)*log x + (b-1)*log (1-x) - logBeta a b
{-# INLINE density #-}

quantile :: Beta -> Double -> Double
quantile d p
  | p < 0 = error $ "probability must be positive. Got " ++ show p
  | p > 1 = error $ "probability must be less than 1. Got " ++ show p
  | otherwise = invertMono (cumulative d) 0 1 p
{-# INLINE quantile #-}
