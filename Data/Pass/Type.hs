{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}
module Data.Pass.Type
  ( Pass(..)
  , env
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

import Control.Category
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Typeable
import Prelude hiding (id,(.),foldr,lookup)
import Data.Pass.Thrist
import Data.Key (foldrWithKey)
import qualified Data.IntMap as IntMap
import Data.Pass.Call
import Data.Pass.Eval
import Data.Pass.Prep
import Data.List (sort)
import Data.Pass.Eval.Naive
import qualified Data.Pass.Env as Env
import Data.Pass.Env (Env)
import Data.Pass.Trans
import Data.Pass.L

data Pass k a b where
  Pass :: (Monoid m, Typeable m) => (m -> o) -> Thrist k i m -> Pass k i o
  L    :: (Ord n, Fractional n) => (n -> o) -> L n n -> Thrist k i n -> Pass k i o
  Ap   :: (b -> c) -> Pass k i (a -> b) -> Pass k i a -> Pass k i c
  Pure :: a -> Pass k i a

instance Trans Pass where
  trans t = Pass id (trans t)

instance Functor (Pass k a) where
  fmap f (L k m i)    = L (f . k) m i
  fmap f (Pass k v)   = Pass (f . k) v
  fmap f (Pure a)     = Pure (f a)
  fmap f (Ap k mf ma) = Ap   (f . k) mf ma

instance Applicative (Pass k a) where
  pure          = Pure
  Pure f <*> xs = fmap f xs
  fs <*> Pure x = fmap ($x) fs
  fs <*> xs     = Ap id fs xs

instance Typeable2 k => Typeable2 (Pass k) where
  typeOf2 (_ :: Pass k a b) = mkTyConApp calcTyCon [typeOf2 (undefined :: k a b)]

calcTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
calcTyCon = mkTyCon3 "pass" "Data.Pass.Type" "Pass"
#else
calcTyCon = mkTyCon "Data.Pass.Type.Pass"
#endif
{-# NOINLINE calcTyCon #-}

instance Prep Pass where
  prep f (Pass k g)   = Pass k (prep f g)
  prep f (L k m i)  = L k m (prep f i)
  prep f (Ap k mf ma) = Ap k (prep f mf) (prep f ma)
  prep _ (Pure a)     = Pure a

instance Num b => Num (Pass k a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  negate = fmap negate
  (*) = liftA2 (*)
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
  naive (L k m i)  as = k $ foldrWithKey step 0 stats
    where
      step g v a = IntMap.findWithDefault 0 g coefs * v + a
      stats = sort $ map (call i) xs
      coefs = callL m $ length xs
      xs = toList as

env :: Call k => Pass k a b -> Env k a
env = envWith Env.empty

envWith :: Call k => Env k a -> Pass k a c -> Env k a
envWith acc Pure{}     = acc
envWith acc (Ap _ l r) = envWith (envWith acc l) r
envWith acc (Pass _ t) = Env.insert t mempty acc

instance Call k => Eval (Pass k) where
  f @@ xs = foldr Env.cons (env f) xs `evalWith` f

evalWith :: Call k => Env k a -> Pass k a c -> c
evalWith _ (Pure a) = a
evalWith g (Ap k l r) = k $ evalWith g l $ evalWith g r
evalWith g (Pass k t) = k $ case Env.lookup t g of
  Nothing -> error "evalWith: missing thrist"
  Just v  -> v
