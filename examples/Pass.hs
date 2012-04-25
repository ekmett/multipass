{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}
module Pass where

import Control.Category
import Control.Applicative
import Control.Newtype
import Data.Monoid
import Data.Hashable
import Data.Foldable
import Data.Typeable
import Prelude hiding (id,(.))
import Data.Pass

-- example calculation type
data Test a b where
  Total  :: Num a => Test a (Sum a)
  Count  :: Num b => Test a (Sum b)
  Square :: Num a => Test a a
  -- Minus  :: Num a => a -> Test a (Sum a)
  deriving Typeable

deriving instance Typeable1 Sum -- :(

count :: (Typeable b, Num b) => Pass Test a b
count = getSum <$> trans Count

total :: (Typeable a, Num a) => Pass Test a a
total = getSum <$> trans Total

mean :: (Typeable a, Fractional a) => Pass Test a a
mean = total / count

sumSq :: (Typeable a, Num a) => Pass Test a a
sumSq = prep Square total

-- E[X^2] - E[X]^2
var :: (Typeable a, Fractional a) => Pass Test a a
var = sumSq/count - mean^2

-- meanAbsDev :: (Typeable a, Fractional a) => Calc Test a a

instance Call Test where
  call Total a = Sum a
  call Count _ = Sum 1
  call Square a = a * a

  equalFun Total Total = True
  equalFun Count Count = True
  equalFun Square Square = True
  equalFun _ _ = False

  hashFunWithSalt n Total = n `hashWithSalt` 0
  hashFunWithSalt n Count = n `hashWithSalt` 1
  hashFunWithSalt n Square = n `hashWithSalt` 2

instance Named Test where
  showsFun _ Total = showString "Total"
  showsFun _ Count = showString "Count"
  showsFun _ Square = showString "Square"
