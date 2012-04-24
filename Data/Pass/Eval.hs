module Data.Pass.Eval
  ( Eval(..)
  ) where

import Data.Typeable

class Typeable2 k => Eval k where
  eval             :: k a b -> a -> b
  hashFunWithSalt  :: Int -> k a b -> Int
  equalFun         :: k a b -> k c d -> Bool
