{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Category
import Control.Newtype
import Control.Monad (liftM)
import Data.Binary
import Data.Foldable
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Typeable
import Data.Pass
import Prelude hiding (id,(.))

-- example calculation type
data Test a b where
  Total    :: Num a => Test a (Sum a)
  Count    :: Test a (Sum Int)
  Square   :: Num a => Test a a
  Minus    :: Double -> Test Double Double
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

instance Binary a => Binary (Min a) where
  put NoMin = put (0 :: Word8)
  put (Min a) = put (1 :: Word8) >> put a
  get = do
    i <- get :: Get Word8
    case i of
      0 -> return NoMin
      1 -> liftM Min get

getMax :: Num a => Max a -> a
getMax (Max a) = a
getMax NoMax = 0

data Max a = Max a | NoMax deriving Typeable

instance Binary a => Binary (Max a) where
  put NoMax = put (0 :: Word8)
  put (Max a) = put (1 :: Word8) >> put a
  get = do
    i <- get :: Get Word8
    case i of
      0 -> return NoMax
      1 -> liftM Max get

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  NoMax `mappend` y = y
  x `mappend` NoMax = x
  Max x `mappend` Max y = Max (max x y)

deriving instance Typeable1 Sum -- :(
deriving instance Binary a => Binary (Sum a)

count :: (Step t, Num b) => t Test a b
count = step $ fromIntegral . getSum <$> trans Count

sumSq :: Step t => t Test Double Double
sumSq = step $ prep Square total

-- E[X^2] - E[X]^2
var :: Step t => t Test Double Double
var = step $ sumSq / count - mean ^ 2

stddev :: Step t => t Test Double Double
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
  showsFun _ Largest   = showString "Largest"
  showsFun _ Smallest  = showString "Smallest"

{-
  putFun Total     = put (0 :: Word8)
  putFun Count     = put (1 :: Word8)
  putFun Square    = put (2 :: Word8)
  putFun (Minus m) = put (3 :: Word8) >> put m
  putFun Abs       = put (4 :: Word8)
  putFun Largest   = put (5 :: Word8)
  putFun Smallest  = put (6 :: Word8)

  equalFun Total Total         = True
  equalFun Count Count         = True
  equalFun Square Square       = True
  equalFun (Minus n) (Minus m) = m == n
  equalFun Abs Abs             = True
  equalFun Largest Largest     = True
  equalFun Smallest Smallest   = True
  equalFun _ _                 = False

  hashFunWithSalt n Total     = n `hashWithSalt` 0
  hashFunWithSalt n Count     = n `hashWithSalt` 1
  hashFunWithSalt n Square    = n `hashWithSalt` 2
  hashFunWithSalt n (Minus m) = n `hashWithSalt` 3 `hashWithSalt` m
  hashFunWithSalt n Abs       = n `hashWithSalt` 4
  hashFunWithSalt n Largest   = n `hashWithSalt` 5
  hashFunWithSalt n Smallest  = n `hashWithSalt` 6
-}


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
