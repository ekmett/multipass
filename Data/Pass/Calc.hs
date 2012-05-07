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
import Data.Pass.L.By
import Data.Pass.Prep
import Data.Pass.Thrist
import Data.Pass.Type
import Data.Pass.Trans
import Data.List (sortBy)
import Data.Function (on)

data Calc k a b where
  Stop :: b -> Calc k a b
  (:&) :: Pass k a b -> (b -> Calc k a c) -> Calc k a c
  Rank :: Ord b => Thrist k a b -> ([Int] -> Calc k a c) -> Calc k a c

infixl 1 :&

instance By (Calc k) where
  by (x :& f) r = by x r :& \b -> f b `by` r
  by (Rank m f) r = Rank m $ \b -> f b `by` r
  by x _ = x

instance Functor (Calc k a) where
  fmap f (Stop b) = Stop (f b)
  fmap f (Rank m k) = Rank m (fmap f . k)
  fmap f (fb :& kba) = fb :& fmap f . kba

instance Applicative (Calc k a) where
  pure = Stop
  Stop f      <*> Stop a      = Stop (f a)
  Stop f      <*> (fb :& kba) = fb :& fmap f . kba
  Stop f      <*> Rank fb kba = Rank fb $ fmap f . kba
  (fg :& kgf) <*> Rank fb kba = fg :& \g -> Rank fb $ \b -> kgf g <*> kba b
  (fg :& kgf) <*> Stop a      = fg :& fmap ($a) . kgf
  (fg :& kgf) <*> (fb :& kba) = liftA2 (,) fg fb :& \(g,b) -> kgf g <*> kba b
  Rank fg kgf <*> (fb :& kba) = fb :& \b -> Rank fg $ \g -> kgf g <*> kba b
  Rank fg kgf <*> Stop a      = Rank fg $ fmap ($a) . kgf
  Rank fg kgf <*> Rank fb kba = Rank fg $ \g -> Rank fb $ \b -> kgf g <*> kba b
  _ *> b = b
  a <* _ = a

instance Monad (Calc k a) where
  return = Stop
  Stop a      >>= f = f a
  (fb :& kba) >>= f = fb :& (>>= f) . kba
  Rank fb kba >>= f = Rank fb $ (>>= f) . kba
  (>>) = (*>)

instance Prep Calc where
  prep _ (Stop b)   = Stop b
  prep t (c :& k)   = prep t c :& prep t . k
  prep t (Rank c k) = Rank (prep t c) (prep t . k)

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
  trans t = trans t :& Stop

instance Call k => Naive (Calc k) where
  naive (Stop b) _ _    = b
  naive (i :& k) n xs   = naive (k $ naive i n xs) n xs
  naive (Rank m k) n xs = naive (k $ map fst $ sortBy (on compare (call m . snd)) $ zip [0..] xs) n xs

instance Call k => Eval (Calc k) where
  eval (Stop b) _ _    = b
  eval (i :& k) n xs   = eval (k (eval i n xs)) n xs
  eval (Rank m k) n xs = eval (k $ map fst $ sortBy (on compare $ call m . snd) $ zip [0..] xs) n xs
