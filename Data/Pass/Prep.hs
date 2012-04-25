{-# LANGUAGE GADTs #-}
module Data.Pass.Prep
  ( Prep(..)
  ) where

import Data.Pass.Thrist

class Prep t where
  prep :: k a b -> t k b c -> t k a c

instance Prep Thrist where
  prep x Nil       = x :- Nil
  prep x (y :- ys) = y :- prep x ys
