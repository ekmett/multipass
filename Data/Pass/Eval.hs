-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Eval
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Eval
  ( Eval(..)
  , (@@)
  ) where

import Data.Foldable

infixl 0 @@

class Eval k where
  -- | Run a calculation
  eval :: k a b -> Int -> [a] -> b

(@@) :: (Eval k, Foldable f) => k a b -> f a -> b
k @@ as = eval k (length xs) xs where xs = toList as
