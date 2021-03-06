-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Calculation
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Calculation
  ( Calculation(..)
  ) where

import Data.Binary
import Data.Monoid
import Data.Typeable
import Data.Pass.Calc
import Data.Pass.Class
import Data.Pass.Eval
import Data.Pass.Fun
import Data.Pass.Thrist
import Data.Pass.Type

class Calculation t where
  calc :: (Eval k, Typeable b, Binary b, Monoid b) => t k a b -> Calc k a b

instance Calculation Fun where
  calc = calc . pass

instance Calculation Thrist where
  calc = calc . pass

instance Calculation Pass where
  calc p = p :& Stop

instance Calculation Calc where
  calc = id
