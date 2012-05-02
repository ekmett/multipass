module Data.Pass.Named
  ( Named(..)
  ) where

import Data.Typeable

infixl 0 `hashFunWithSalt`

class Typeable2 k => Named k where
  showsFun         :: Int -> k a b -> String -> String
  hashFunWithSalt  :: Int -> k a b -> Int
  equalFun         :: k a b -> k c d -> Bool
