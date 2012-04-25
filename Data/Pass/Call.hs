module Data.Pass.Call
  ( Call(..)
  ) where

import Data.Typeable

class Typeable2 k => Call k where
  call             :: k a b -> a -> b
  hashFunWithSalt  :: Int -> k a b -> Int
  equalFun         :: k a b -> k c d -> Bool
