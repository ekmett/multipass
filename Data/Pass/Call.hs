-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Call
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------


module Data.Pass.Call
  ( Call(..)
  ) where

import Data.Pass.Named

class Named k => Call k where
  call :: k a b -> a -> b
