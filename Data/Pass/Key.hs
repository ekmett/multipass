{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Data.Pass.Key
  ( Key(..)
  ) where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Pass.Call
import Data.Hashable
import Data.Monoid
import Data.Pass.Thrist
import Data.Pass.Named

data Key k a where
  Key :: (Typeable b, Monoid b) => Thrist k a b -> Key k a

instance Call k => Eq (Key k a) where
  Key (x :: Thrist k a b) == Key (y :: Thrist k a c) = equalFun x y && typeOf (undefined :: b) == typeOf (undefined :: c)

instance Call k => Hashable (Key k a) where
  hashWithSalt s (Key (tkab :: Thrist k a b)) = s `hashFunWithSalt` tkab `hashWithSalt` typeOf (undefined :: b)

instance Named k => Show (Key k a) where
  showsPrec d (Key t) = showParen (d > 10) $ showString "Key " . showsFun 10 t
