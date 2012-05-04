module Data.Pass.L.By
  (By(..)) where

import Data.Pass.L.Estimator

class By k where
  by :: k a b -> Estimator -> k a b
