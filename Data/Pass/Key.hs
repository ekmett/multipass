{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Data.Pass.Key
  ( Key(..)
  ) where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Pass.Eval
import Data.Hashable
import Data.Monoid
import Data.Pass.Thrist

data Key k a where
  Key :: (Typeable b, Monoid b) => Thrist k a b -> Key k a

instance Eval k => Eq (Key k a) where
  Key x == Key y = equalFun x y

instance Eval k => Hashable (Key k a) where
  hashWithSalt s (Key (tkab :: Thrist k a b)) = s `hashFunWithSalt` tkab `hashWithSalt` typeOf (undefined :: b)
