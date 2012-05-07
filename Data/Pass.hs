-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass
-- Copyright   :  (C) 2012 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
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

  -- * Input conditioning
  , Prep(..)

  -- * L-Estimators
  , L(..)
  , (@#)
  , breakdown
  , iqm -- interquantile mean
  , idm -- interdecile mean

  -- ** Quantile estimators
  , Estimator(..)
  , By(..)

  -- ** Robust statistics based on L-estimators
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

  -- ** Classes required for user-defined calculation types
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
