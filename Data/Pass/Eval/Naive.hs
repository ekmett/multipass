-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Eval.Naive
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Eval.Naive
  ( Naive(..)
  , (@@@)
  ) where

import Data.Foldable

infixl 0 @@@

class Naive k where
  naive :: k a b -> Int -> [a] -> b

(@@@) :: (Naive k, Foldable f) => k a b -> f a -> b
k @@@ as = naive k (length xs) xs where xs = toList as
