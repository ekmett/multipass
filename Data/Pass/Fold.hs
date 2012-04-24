{-# LANGUAGE TypeFamilies, KindSignatures, DataKinds, GADTs, DeriveDataTypeable, StandaloneDeriving, ExtendedDefaultRules #-}
module Data.Pass.Fold
  ( Fold(..)
  , naiveFold
  ) where

import Control.Category
import Control.Applicative
import Prelude hiding (id,(.))
import Data.Pass.Calc
import Data.Pass.Eval

data Fold k a b where
  Stop :: b -> Fold k a b
  Step :: Fold k a b -> (b -> Calc k a c) -> Fold k a c

instance Functor (Fold k a) where
  fmap f (Stop b) = Stop (f b)
  fmap f (Step fb kba) = Step fb (fmap f . kba)

instance Applicative (Fold k a) where
  pure = Stop
  Stop f      <*> Stop a      = Stop (f a)
  Stop f      <*> Step fb kba = Step fb (fmap f . kba)
  Step fg kgf <*> Stop a      = Step fg (fmap ($a) . kgf)
  Step fg kgf <*> Step fb kba = Step (liftA2 (,) fg fb) $ \(g, b) -> kgf g <*> kba b

instance Num b => Num (Fold k a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Fold k a b) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (Fold k a b) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

naiveFold :: Eval k => Fold k a b -> [a] -> b
naiveFold (Stop b) _    = b
naiveFold (Step i k) as = naiveCalc (k $ naiveFold i as) as
