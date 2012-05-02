module Data.Pass.Step
  ( Step(..)
  , midhinge
  , trimean
  , iqr
  , iqm
  ) where

import Data.Pass.Type
import Data.Pass.Calc
import Data.Pass.Prep
import Data.Pass.Robust
import Data.Pass.Accelerated

class Prep t => Step t where
  step :: Pass k a b -> t k a b

instance Step Pass where
  step = id

instance Step Calc where
  step k = Step (Stop ()) (const k)

midhinge :: Step t => t k Double Double
midhinge = step $ (q1 + q3) / 2

-- | Tukey's trimean
trimean :: Step t => t k Double Double
trimean = step $ (q1 + 2 * q2 + q3) / 4

-- | interquartile range
iqr :: Step t => t k Double Double
iqr = step $ q3 - q1

-- | interquartile mean
iqm :: Step t => t k Double Double
iqm = step $ trimmed 0.25 mean
