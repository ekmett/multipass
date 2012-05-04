module Data.Pass.Accelerant
  ( Accelerant(..)
  ) where

import Data.Typeable
import Data.Pass.Type
import Data.Pass.L
import Data.Pass.Robust

-- provide hooks to allow the user to accelerate non-robust L-estimators
class Accelerant k where
  meanPass :: (Typeable a, Fractional a, Ord a) => Pass k a a
  meanPass = robust LMean

  totalPass :: (Typeable a, Fractional a, Ord a) => Pass k a a
  totalPass = robust LTotal

  largestPass :: (Typeable a, Num a, Ord a) => Pass k a a
  largestPass = robust $ NthLargest 0

  smallestPass :: (Typeable a, Num a, Ord a) => Pass k a a
  smallestPass = robust $ NthSmallest 0

  midrangePass :: (Typeable a, Num a, Ord a) => Pass k a a
  midrangePass = largestPass - smallestPass
