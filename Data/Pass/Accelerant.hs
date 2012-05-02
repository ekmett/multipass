module Data.Pass.Accelerant
  ( Accelerant(..)
  ) where

import Data.Pass.Type
import Data.Pass.L
import Data.Pass.Robust

-- provide hooks to allow the user to accelerate non-robust L-estimators
class Accelerant k where
  meanPass :: Pass k Double Double
  meanPass = robust LMean

  totalPass :: Pass k Double Double
  totalPass = robust LTotal
