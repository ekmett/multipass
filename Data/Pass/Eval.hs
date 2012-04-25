module Data.Pass.Eval
  ( Eval(..)
  , (@@)
  ) where

import Data.Foldable

class Eval k where
  eval :: Foldable f => k a b -> f a -> b

infixr 5 @@

(@@) :: (Eval k, Foldable f) => k a b -> f a -> b
(@@) = eval
