{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Data.Pass.Fold.Key
  ( FoldKey(..)
  ) where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Pass.Eval
import Data.Hashable

data FoldKey k a where
  FoldKey :: Typeable b => k a b -> FoldKey k a

instance Eval k => Eq (FoldKey k a) where
  FoldKey x == FoldKey y = equalFun x y

instance Eval k => Hashable (FoldKey k a) where
  hashWithSalt s (FoldKey (kab :: k a b)) = s `hashFunWithSalt` kab `hashWithSalt` typeOf (undefined :: b)
