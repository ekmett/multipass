{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Main where


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
import Data.Pass.Monoid.Ord
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

deriving instance Typeable1 Sum -- :(
deriving instance Binary a => Binary (Sum a)

count :: (Step t, Num b) => t Test a b
count = step $ fromIntegral . getSum <$> trans Count

sumSq :: Step t => t Test Double Double
sumSq = step $ Square `prep` total

-- | @E[X^2] - E[X]^2@
var :: Step t => t Test Double Double
var = step $ sumSq / count - mean ^ 2

stddev :: Step t => t Test Double Double
stddev = step $ sqrt var

-- > absDev median -- median absolute deviation
-- > absDev mean   -- mean absolute deviation
absdev :: Pass Test Double Double -> Calc Test Double Double
absdev mu = mu :& \m -> Minus m `prep` Abs `prep` step mu

-- median absolute deviation
mad :: Calc Test Double Double
mad = absdev median

instance Named Test where
  showsFun _ Total     = showString "Total"
  showsFun _ Count     = showString "Count"
  showsFun _ Square    = showString "Square"
  showsFun d (Minus n) = showParen (d > 10) $ showString "Minus " . showsPrec 10 n
  showsFun _ Abs       = showString "Abs"
  showsFun _ Largest   = showString "Largest"
  showsFun _ Smallest  = showString "Smallest"

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
(@!) = (@@)
