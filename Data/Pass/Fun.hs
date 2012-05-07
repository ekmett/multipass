{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Data.Pass.Fun
  ( Fun(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Data.Hashable
import Data.Typeable
import Data.Pass.Named
import Data.Pass.Call
import Data.Pass.Trans
import Data.Pass.L.By
import Data.Pass.Eval
import Data.Pass.Eval.Naive

newtype Fun k a b = Fun { unFun :: k a b }

instance Trans Fun where
  trans = Fun

instance Named k => Show (Fun k a b) where
  showsPrec = showsFun

instance Typeable2 k => Typeable2 (Fun k) where
  typeOf2 (_ :: Fun k a b) = mkTyConApp funTyCon [typeOf2 (undefined :: k a b)]

funTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
funTyCon = mkTyCon3 "pass" "Data.Pass.Fun" "Fun"
#else
funTyCon = mkTyCon "Data.Pass.Fun.Fun"
#endif
{-# NOINLINE funTyCon #-}

instance Named k => Named (Fun k) where
  showsFun d (Fun f) = showParen (d > 10) $ showString "Fun " . showsFun 10 f
  hashFunWithSalt i (Fun k) = hashFunWithSalt i k
  equalFun (Fun f) (Fun g) = equalFun f g
  putFun (Fun f) = putFun f

instance Call k => Call (Fun k) where
  call (Fun k) = call k

instance Named k => Eq (Fun k a b) where
  (==) = equalFun

instance Named k => Hashable (Fun k a b) where
  hashWithSalt = hashFunWithSalt

instance Naive k => Naive (Fun k) where
  naive (Fun k) = naive k

instance Eval k => Eval (Fun k) where
  eval (Fun k) = eval k

instance By k => By (Fun k) where
  by (Fun k) r = Fun (by k r)
