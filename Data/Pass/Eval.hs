module Data.Pass.Eval
  ( Eval(..)
  ) where

import Data.Foldable

infixl 0 @@

class Eval k where
  (@@) :: Foldable f => k a b -> f a -> b

