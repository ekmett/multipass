module Data.Pass.Eval
  ( Eval(..)
  ) where

import Data.Foldable

infixr 5 @@

class Eval k where
  (@@) :: Foldable f => k a b -> f a -> b

