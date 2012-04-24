{-# LANGUAGE TypeFamilies, KindSignatures, DataKinds, GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}
module Data.Pass.Fun
  ( Fun(..)
  ) where

import Data.Hashable
import Data.Typeable
import Data.Pass.Eval
import Data.Pass.Named

newtype Fun (k :: * -> * -> *) a b = Fun { unFun :: k a b }

instance Named k => Show (Fun k a b) where
  showsPrec = showsFun

instance Typeable2 k => Typeable2 (Fun k) where
  typeOf2 tkab = mkTyConApp funTyCon [typeOf2 (kab tkab)]
    where kab :: t k a b -> k a b
          kab = undefined

funTyCon :: TyCon
funTyCon = mkTyCon3 "pass" "Data.Pass.Eval" "Fun"
{-# NOINLINE funTyCon #-}

instance Eval k => Eval (Fun k) where
  eval (Fun k) = eval k
  hashFunWithSalt i (Fun k) = hashFunWithSalt i k
  equalFun (Fun f) (Fun g) = equalFun f g

instance Named k => Named (Fun k) where
  showsFun d (Fun f) = showParen (d > 10) $ showString "Fun " . showsFun 10 f

instance Eval k => Eq (Fun k a b) where
  (==) = equalFun

instance Eval k => Hashable (Fun k a b) where
  hashWithSalt = hashFunWithSalt
