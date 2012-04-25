{-# LANGUAGE TypeFamilies, KindSignatures, DataKinds, GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}
module Main where

import Control.Category
import Control.Applicative
import Control.Newtype
import Data.Monoid
import Data.Hashable
import Data.Foldable
import Data.Typeable
import Prelude hiding (id,(.))
import Data.Pass.Eval
import Data.Pass.Thrist
import Data.Pass.Calc
import Data.Pass.Fold
import Data.Pass.Named

-- example calculation type
data Test a b where
  Total  :: Num a => Test a (Sum a)
  Count  :: Num b => Test a (Sum b)
  Square :: Num a => Test a a
  deriving Typeable

deriving instance Typeable1 Sum

calc :: (Monoid b, Typeable b) => Test a b -> Calc Test a b
calc t = Calc id (thrist t)

count :: (Typeable b, Num b) => Calc Test a b
count = getSum <$> calc Count

total :: (Typeable a, Num a) => Calc Test a a
total = getSum <$> calc Total

mean :: (Typeable a, Fractional a) => Calc Test a a
mean = total / count

sumSq :: (Typeable a, Num a) => Calc Test a a
sumSq = thrist Square `lmap` total

var :: (Typeable a, Fractional a) => Calc Test a a
var = sumSq/count - mean^2

instance Eval Test where
  equalFun Total Total = True
  equalFun Count Count = True
  equalFun Square Square = True
  equalFun _ _ = False
  hashFunWithSalt n Total = n `hashWithSalt` 0
  hashFunWithSalt n Count = n `hashWithSalt` 1
  hashFunWithSalt n Square = n `hashWithSalt` 2
  eval Total a = Sum a
  eval Count _ = Sum 1
  eval Square a = a * a

instance Named Test where
  showsFun _ Total = showString "Total"
  showsFun _ Count = showString "Count"
  showsFun _ Square = showString "Square"
