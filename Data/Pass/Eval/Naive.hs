module Data.Pass.Eval.Naive
  ( Naive(..)
  ) where

import Data.Foldable

class Naive k where
  naive :: Foldable f => k a b -> f a -> b
