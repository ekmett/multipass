-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.L.By
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.L.By
  (By(..)) where

import Data.Pass.L.Estimator

class By k where
  by :: k a b -> Estimator -> k a b
