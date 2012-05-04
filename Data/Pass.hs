module Data.Pass
  (
  -- * Evaluation
    Eval(..)
  , Naive(..)

  -- * Single pass calculations
  , Pass(..)
  , Passable(..)

  -- ** Embedding single pass calculations
  , Step(..)

  -- * Multipass calculations
  , Calc(..)
  , Calculation(..)
  , passes
  , (@@@)

  -- * Input conditioning
  , Prep(..)

  -- * L-Estimators (calculations based on order statistics)
  , L(..)
  , (@#)
  , breakdown
  , iqm

  -- ** Quantile estimators
  , Estimator(..)
  , By(..)

  -- ** Robust statistics
  , Robust(..)
  , median
  , tercile, t1, t2
  , quartile, q1, q2, q3
  , quintile, qu1, qu2, qu3, qu4
  , percentile
  , permille

  -- ** Acceleration for non-robust L-estimators
  , Accelerated(..)

  -- * Implementation Details
  , Thrist(..)
  , thrist
  , Trans(..)

  -- ** Classes required for user-defined calculations
  , Call(..)
  , Named(..)
  , Accelerant(..)
  ) where

import Data.Pass.Calc
import Data.Pass.Accelerant
import Data.Pass.Accelerated
import Data.Pass.Calculation
import Data.Pass.Call
import Data.Pass.Class
import Data.Pass.Eval
import Data.Pass.Eval.Naive
import Data.Pass.Named
import Data.Pass.Prep
import Data.Pass.Thrist
import Data.Pass.Trans
import Data.Pass.Type
import Data.Pass.Step
import Data.Pass.L
import Data.Pass.L.By
import Data.Pass.L.Estimator
import Data.Pass.Robust
