{-# LANGUAGE GADTs #-}
module Data.Pass.Calc
  ( Calc(..)
  ) where

import Control.Category
import Control.Applicative
import Prelude hiding (id,(.))
import Data.Pass.Call
import Data.Pass.Eval
import Data.Pass.Eval.Naive
import Data.Pass.Prep
import Data.Pass.Type
import Data.Pass.Trans

data Calc k a b where
  Stop :: b -> Calc k a b
  Step :: Calc k a b -> (b -> Pass k a c) -> Calc k a c

instance Functor (Calc k a) where
  fmap f (Stop b) = Stop (f b)
  fmap f (Step fb kba) = Step fb (fmap f . kba)

instance Applicative (Calc k a) where
  pure = Stop
  Stop f      <*> Stop a      = Stop (f a)
  Stop f      <*> Step fb kba = Step fb (fmap f . kba)
  Step fg kgf <*> Stop a      = Step fg (fmap ($a) . kgf)
  Step fg kgf <*> Step fb kba = Step (liftA2 (,) fg fb) $ \(g, b) -> kgf g <*> kba b

instance Prep Calc where
  prep _ (Stop b) = Stop b
  prep t (Step c k) = Step (prep t c) (prep t . k)

instance Num b => Num (Calc k a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Calc k a b) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (Calc k a b) where
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

instance Trans Calc where
  trans t = Step (Stop ()) $ \_ -> trans t

instance Call k => Naive (Calc k) where
  naive (Stop b) _    = b
  naive (Step i k) as = naive (k (naive i as)) as

instance Call k => Eval (Calc k) where
  eval (Stop b)   _  = b
  eval (Step i k) xs = eval (k (eval i xs)) xs