-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Step
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Step
  ( Step(..)
  ) where

import Data.Pass.Type
import Data.Pass.Calc
import Data.Pass.Prep

class Prep t => Step t where
  step :: Pass k a b -> t k a b

instance Step Pass where
  step = id

instance Step Calc where
  step k = k :& Stop
