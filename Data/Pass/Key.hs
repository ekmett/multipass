{-# LANGUAGE ScopedTypeVariables, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Key
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------


module Data.Pass.Key
  ( Key(..)
  ) where

import Prelude hiding (lookup)
import Data.Binary
import Data.Typeable
import Data.Hashable
import Data.Monoid
import Data.Pass.Thrist
import Data.Pass.Named

data Key k a where
  Key :: (Typeable b, Binary b, Monoid b) => Thrist k a b -> Key k a

instance Named k => Eq (Key k a) where
  Key (x :: Thrist k a b) == Key (y :: Thrist k a c) = equalFun x y && typeOf (undefined :: b) == typeOf (undefined :: c)

instance Named k => Hashable (Key k a) where
  hashWithSalt s (Key (tkab :: Thrist k a b)) = s `hashFunWithSalt` tkab `hashWithSalt` typeOf (undefined :: b)

instance Named k => Show (Key k a) where
  showsPrec d (Key t) = showParen (d > 10) $ showString "Key " . showsFun 10 t
