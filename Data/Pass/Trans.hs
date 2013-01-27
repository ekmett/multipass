-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Trans
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Trans
  ( Trans(..)
  ) where

import Data.Typeable
import Data.Monoid
import Data.Binary
-- import Data.Pass.Eval

class Trans t where
  trans :: (Binary b, Monoid b, Typeable b) => k a b -> t k a b
