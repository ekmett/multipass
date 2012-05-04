{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}

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
  Total    :: Num a => Test a (Sum a)
  Count    :: Test a (Sum Int)
  Square   :: Num a => Test a a
  Minus    :: (Typeable a, Show a, Num a, Eq a, Hashable a) => a -> Test a a
  Abs      :: Num a => Test a a
  Smallest :: Num a => Test a (Min a)
  Largest  :: Num a => Test a (Max a)
  deriving Typeable

data Min a = Min a | NoMin deriving Typeable

getMin :: Num a => Min a -> a
getMin (Min a) = a
getMin NoMin = 0

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  NoMin `mappend` y = y
  x `mappend` NoMin = x
  Min x `mappend` Min y = Min (min x y)

getMax :: Num a => Max a -> a
getMax (Max a) = a
getMax NoMax = 0

data Max a = Max a | NoMax deriving Typeable

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  NoMax `mappend` y = y
  x `mappend` NoMax = x
  Max x `mappend` Max y = Max (max x y)

deriving instance Typeable1 Sum -- :(

count :: (Step t, Num b) => t Test a b
count = step $ fromIntegral . getSum <$> trans Count

sumSq :: (Step t, Fractional a, Ord a, Typeable a) => t Test a a
sumSq = step $ prep Square total

-- E[X^2] - E[X]^2
var :: (Step t, Fractional a, Ord a, Typeable a) => t Test a a
var = step $ sumSq / count - mean ^ 2

stddev :: (Step t, Floating a, Ord a, Typeable a) => t Test a a
stddev = step $ sqrt var

-- > absDev median -- median absolute deviation
-- > absDev mean   -- mean absolute deviation
absdev :: (Show a, Fractional a, Ord a, Typeable a, Hashable a, Eq a) => Pass Test a a -> Calc Test a a
absdev mu = step mu `Step` \m -> Minus m `prep` Abs `prep` mu

instance Named Test where
  showsFun _ Total     = showString "Total"
  showsFun _ Count     = showString "Count"
  showsFun _ Square    = showString "Square"
  showsFun d (Minus n) = showParen (d > 10) $ showString "Minus " . showsPrec 10 n
  showsFun _ Abs       = showString "Abs"
  showsFun _ Largest   = showString "Largest"
  showsFun _ Smallest  = showString "Smallest"

  equalFun Total Total         = True
  equalFun Count Count         = True
  equalFun Square Square       = True
  equalFun (Minus n) (Minus m) = cast m == Just n
  equalFun Abs Abs             = True
  equalFun Largest Largest     = True
  equalFun Smallest Smallest   = True
  equalFun _ _                 = False

  hashFunWithSalt n Total     = n `hashWithSalt` 0
  hashFunWithSalt n Count     = n `hashWithSalt` 1
  hashFunWithSalt n Square    = n `hashWithSalt` 2
  hashFunWithSalt n (Minus m) = n `hashWithSalt` 3 `hashWithSalt` m `hashWithSalt` typeOf m
  hashFunWithSalt n Abs       = n `hashWithSalt` 4
  hashFunWithSalt n Largest   = n `hashWithSalt` 5
  hashFunWithSalt n Smallest  = n `hashWithSalt` 6

instance Call Test where
  call Total a = Sum a
  call Count _ = Sum 1
  call Square a = a * a
  call (Minus n) a = a - n
  call Abs a = abs a
  call Largest a = Max a
  call Smallest a = Min a

instance Accelerant Test where
  meanPass  = total / count
  totalPass = getSum <$> trans Total
  largestPass = getMax <$> trans Largest
  smallestPass = getMin <$> trans Smallest

infixl 0 @!

(@!) :: Calc Test a b -> [a] -> b
(@!) = naive
