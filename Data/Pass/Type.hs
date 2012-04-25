{-# LANGUAGE GADTs #-}
module Data.Pass.Type
  ( Pass(..)
  , env
  ) where

import Control.Category
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Typeable
import Prelude hiding (id,(.),foldr,lookup)
import Data.Pass.Thrist
import Data.Pass.Call
import Data.Pass.Eval
import Data.Pass.Prep
import Data.Pass.Eval.Naive
import qualified Data.Pass.Env as Env
import Data.Pass.Env (Env)
import Data.Pass.Trans

data Pass k a b where
  Pass :: (Monoid m, Typeable m) => (m -> o) -> Thrist k i m -> Pass k i o
  Ap   :: (b -> c) -> Pass k i (a -> b) -> Pass k i a -> Pass k i c
  Pure :: a -> Pass k i a

instance Trans Pass where
  trans t = Pass id (trans t)

instance Functor (Pass k a) where
  fmap f (Pass g v) = Pass (f . g) v
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap g mf ma) = Ap (f . g) mf ma

instance Applicative (Pass k a) where
  pure = Pure
  (<*>) = Ap id

instance Typeable2 k => Typeable2 (Pass k) where
  typeOf2 tkab = mkTyConApp calcTyCon [typeOf2 (kab tkab)]
    where kab :: t k a b -> k a b
          kab = undefined

calcTyCon :: TyCon
calcTyCon = mkTyCon3 "pass" "Data.Pass.Pass" "Pass"
{-# NOINLINE calcTyCon #-}

instance Prep Pass where
  prep f (Pass k g)   = Pass k (g . thrist f)
  prep f (Ap k mf ma) = Ap k (prep f mf) (prep f ma)
  prep _ (Pure a)     = Pure a

instance Num b => Num (Pass k a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Pass k a b) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (Pass k a b) where
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

instance Call k => Naive (Pass k) where
  naive (Pure b)     _  = b
  naive (Ap k mf mx) as = k $ naive mf as $ naive mx as
  naive (Pass k t)   as = k $ foldMap (call t) as

env :: Call k => Pass k a b -> Env k a
env = envWith Env.empty

envWith :: Call k => Env k a -> Pass k a c -> Env k a
envWith acc Pure{}     = acc
envWith acc (Ap _ l r) = envWith (envWith acc l) r
envWith acc (Pass _ t) = Env.insert t mempty acc

instance Call k => Eval (Pass k) where
  eval f xs = foldr Env.cons (env f) xs `evalWith` f

evalWith :: Call k => Env k a -> Pass k a c -> c
evalWith _ (Pure a) = a
evalWith g (Ap k l r) = k $ evalWith g l $ evalWith g r
evalWith g (Pass k t) = k $ case Env.lookup t g of
  Nothing -> error "evalWith: missing thrist"
  Just v  -> v
