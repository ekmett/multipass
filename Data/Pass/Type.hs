{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Type
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Type
  ( Pass(..)
  , env
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Category
import Control.Applicative hiding (empty)
import Data.Binary
import Data.Monoid
import Data.Foldable
import Data.Typeable
import Data.List (sort)
import Data.Key (foldrWithKey)
import qualified Data.IntMap as IntMap
import Prelude hiding (id,(.),foldr,lookup)

import Data.Pass.Call
import Data.Pass.Eval
import Data.Pass.Eval.Naive
import qualified Data.Pass.Env as Env
import Data.Pass.Env (Env)
import Data.Pass.Prep
import Data.Pass.Thrist
import Data.Pass.Trans
import Data.Pass.L
import Data.Pass.L.By

data Pass k a b where
  Pass :: (Typeable m, Binary m, Monoid m) => (m -> o) -> Thrist k i m -> Pass k i o
  L    :: (n -> o) -> L n n -> Thrist k i n -> Pass k i o
  Ap   :: (b -> c) -> Pass k i (a -> b) -> Pass k i a -> Pass k i c
  Pure :: a -> Pass k i a

instance By (Pass k) where
  by (Ap k mf ma) r = Ap k (by mf r) (by ma r)
  by (L k m i) r = L k (by m r) i
  by x _ = x

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

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 704
instance Show (Pass k a b) where
  showsPrec _ _ = showString "<pass>"
instance Eq (Pass k a b) where
  _ == _ = False
#endif
#endif

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
  naive (Pure b)     _ _  = b
  naive (Ap k mf mx) n xs = k $ naive mf n xs $ naive mx n xs
  naive (Pass k t)   _ xs = k $ foldMap (call t) xs
  naive (L k m i)    n xs = k $ ordL m $ foldrWithKey step 0 stats
    where
      step g v x = ordL m $ IntMap.findWithDefault 0 g coefs * v + x
      stats = ordL m $ sort $ call i <$> xs
      coefs = callL m n

env :: Call k => Pass k a b -> Env k a
env = envWith Env.empty

envWith :: Call k => Env k a -> Pass k a c -> Env k a
envWith acc (Ap _ l r) = envWith (envWith acc l) r
envWith acc (Pass _ t) = Env.insert t mempty acc
envWith acc _ = acc

instance Call k => Eval (Pass k) where
  eval f n xs = evalWith (foldr Env.cons (env f) xs) f n xs

evalWith :: Call k => Env k a -> Pass k a c -> Int -> [a] -> c
evalWith _ (Pure a)   _ _  = a
evalWith g (Ap k l r) n xs = k $ evalWith g l n xs $ evalWith g r n xs
evalWith _ (L k m i)  n xs = k $ eval m n $ map (call i) xs
evalWith g (Pass k t) _ _  = k $ case Env.lookup t g of
  Nothing -> error "evalWith: missing thrist"
  Just v  -> v
