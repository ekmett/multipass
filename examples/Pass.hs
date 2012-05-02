{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}
module Pass where

import Control.Category
import Control.Applicative
import Control.Newtype
import Data.Monoid
import Data.Hashable
import Data.Foldable
import Data.Typeable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Prelude hiding (id,(.))
import Data.Pass

-- example calculation type
data Test a b where
  Total  :: Num a => Test a (Sum a)
  Count  :: Num b => Test a (Sum b)
  Square :: Num a => Test a a
  Minus  :: Double -> Test Double Double
  Abs    :: Num a => Test a a
  deriving Typeable

deriving instance Typeable1 Sum -- :(

count :: (Step t, Typeable b, Num b) => t Test a b
count = step $ getSum <$> trans Count

sumSq :: (Step t, Typeable a, Num a) => t Test a a
sumSq = prep Square total

-- E[X^2] - E[X]^2
var :: (Step t, Typeable a, Fractional a) => t Test a a
var = step $ sumSq / count - mean ^ 2

stddev :: (Step t, Typeable a, Floating a) => t Test a a
stddev = step $ sqrt var

-- > absDev median -- median absolute deviation
-- > absDev mean   -- mean absolute deviation
absdev :: Pass Test Double Double -> Calc Test Double Double
absdev mu = step mu `Step` \m -> Minus m `prep` Abs `prep` mu

instance Named Test where
  showsFun _ Total     = showString "Total"
  showsFun _ Count     = showString "Count"
  showsFun _ Square    = showString "Square"
  showsFun d (Minus n) = showParen (d > 10) $ showString "Minus " . showsPrec 10 n
  showsFun _ Abs       = showString "Abs"

  equalFun Total Total         = True
  equalFun Count Count         = True
  equalFun Square Square       = True
  equalFun (Minus n) (Minus m) = cast m == Just n
  equalFun Abs Abs             = True
  equalFun _ _                 = False

  hashFunWithSalt n Total     = n `hashWithSalt` 0
  hashFunWithSalt n Count     = n `hashWithSalt` 1
  hashFunWithSalt n Square    = n `hashWithSalt` 2
  hashFunWithSalt n (Minus m) = n `hashWithSalt` 3 `hashWithSalt` m
  hashFunWithSalt n Abs       = n `hashWithSalt` 4
  hashFunWithSalt n Median    = n `hashWithSalt` 5

instance Call Test where
  call Total a = Sum a
  call Count _ = Sum 1
  call Square a = a * a
  call (Minus n) a = a - n
  call Abs a = abs a

instance Accelerant Test where
  totalPass = getSum <$> trans Total
  meanPass  = total / count
