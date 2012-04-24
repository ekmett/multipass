{-# LANGUAGE TypeFamilies, KindSignatures, GADTs #-}
module Data.Pass.Calc
  ( Calc(..)
  , lmap
  , naiveCalc
  , env
  , (@@)
  ) where

import Control.Category
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Typeable
import Prelude hiding (id,(.),foldr,lookup)
import Data.Pass.Thrist
import Data.Pass.Eval
import qualified Data.Pass.Env as Env
import Data.Pass.Env (Env)

data Calc :: (* -> * -> *) -> * -> * -> * where
  -- TODO: after the thrist store a reducer?
  Calc :: (Monoid m, Typeable m) => (m -> o) -> Thrist k i m -> Calc k i o
  Ap   :: (b -> c) -> Calc k i (a -> b) -> Calc k i a -> Calc k i c
  Pure :: a -> Calc k i a

instance Functor (Calc k a) where
  fmap f (Calc g v) = Calc (f . g) v
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap g mf ma) = Ap (f . g) mf ma

instance Applicative (Calc k a) where
  pure = Pure
  (<*>) = Ap id

instance Typeable2 k => Typeable2 (Calc k) where
  typeOf2 tkab = mkTyConApp calcTyCon [typeOf2 (kab tkab)]
    where kab :: t k a b -> k a b
          kab = undefined

calcTyCon :: TyCon
calcTyCon = mkTyCon3 "pass" "Data.Pass.Calc" "Calc"
{-# NOINLINE calcTyCon #-}

lmap :: Thrist k a b -> Calc k b c -> Calc k a c
lmap f (Calc k g)   = Calc k (g . f)
lmap f (Ap k mf ma) = Ap k (lmap f mf) (lmap f ma)
lmap _ (Pure a)     = Pure a

instance Num b => Num (Calc k a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
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

-- naive evaluation for testing
naiveCalc :: (Eval k, Foldable f) => Calc k a b -> f a -> b
naiveCalc (Pure b)     _  = b
naiveCalc (Ap k mf mx) as = k $ naiveCalc mf as $ naiveCalc mx as
naiveCalc (Calc k t)   as = k $ foldMap (eval t) as

env :: Eval k => Calc k a b -> Env k a
env = env' Env.empty

env' :: Eval k => Env k a -> Calc k a c -> Env k a
env' acc Pure{}     = acc
env' acc (Ap _ l r) = env' (env' acc l) r
env' acc (Calc _ t) = Env.insert t mempty acc

(@@) :: (Eval k, Foldable f) => Calc k a b -> f a -> b
f @@ xs = foldr Env.cons (env f) xs `evalWith` f

evalWith :: Eval k => Env k a -> Calc k a c -> c
evalWith _ (Pure a) = a
evalWith g (Ap k l r) = k $ evalWith g l $ evalWith g r
evalWith g (Calc k t) = k $ case Env.lookup t g of
  Nothing -> error "evalWith: missing thrist"
  Just v  -> v
