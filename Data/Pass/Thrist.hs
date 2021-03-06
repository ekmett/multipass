{-# LANGUAGE CPP, GADTs, Rank2Types, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Thrist
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Thrist
  ( Thrist(..)
  , thrist
  , fromThrist
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Category
import Data.Hashable
import Data.Typeable
import Data.Binary
import Prelude hiding (id,(.))
import Data.Pass.Call
import Data.Pass.Named
import Data.Pass.Trans
import Data.Pass.L.By

infixr 5 :-

data Thrist k a b where
  Nil :: Thrist k a a
  (:-) :: k b c -> Thrist k a b -> Thrist k a c

instance Named k => Show (Thrist k a b) where
  showsPrec d xs = showsFun d xs

instance Trans Thrist where
  trans k = k :- Nil

instance Category (Thrist k) where
  id = Nil
  Nil . x = x
  (f :- xs) . ys = f :- (xs . ys)

thrist :: k a b -> Thrist k a b
thrist k = k :- Nil
{-# INLINE thrist #-}

instance Named k => Named (Thrist k) where
  showsFun d (x :- xs) = showParen (d > 5) $ showsFun 6 x . showString " :- " . showsFun 5 xs
  showsFun _ Nil = showString "Nil"

  hashFunWithSalt k Nil = k
  hashFunWithSalt k (f :- xs) = k `hashFunWithSalt` f `hashWithSalt` xs

  equalFun Nil Nil = True
  equalFun (a :- as) (b :- bs) = equalFun a b && equalFun as bs
  equalFun _ _ = False

  putFun Nil = put (0 :: Word8)
  putFun (x :- xs) = do
    put (1 :: Word8)
    putFun x
    putFun xs

instance Call k => Call (Thrist k) where
  call Nil = id
  call (f :- xs) = call f . call xs

instance By k => By (Thrist k) where
  by Nil _ = Nil
  by (x :- xs) r = by x r :- by xs r

fromThrist :: forall k a b c. Call k => (forall d e. k d e -> c) -> Thrist k a b -> [c]
fromThrist f = go where
  go :: Thrist k f g -> [c]
  go Nil = []
  go (x :- xs) = f x : go xs
{-# INLINE fromThrist #-}

instance Named k => Eq (Thrist k a b) where
  (==) = equalFun

instance Named k => Hashable (Thrist k a b) where
  hashWithSalt = hashFunWithSalt

instance Typeable2 k => Typeable2 (Thrist k) where
  typeOf2 (_ :: Thrist k a b) = mkTyConApp thristTyCon [typeOf2 (undefined :: k a b)]

thristTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
thristTyCon = mkTyCon3 "pass" "Data.Pass.Thrist" "Thrist"
#else
thristTyCon = mkTyCon "Data.Pass.Thrist.Thrist"
#endif
{-# NOINLINE thristTyCon #-}
