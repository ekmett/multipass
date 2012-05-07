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
