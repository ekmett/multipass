module Data.Pass.Calculation
  ( Calculation(..)
  ) where

import Data.Typeable
import Data.Monoid
import Data.Pass.Calc
import Data.Pass.Class
import Data.Pass.Eval
import Data.Pass.Fun
import Data.Pass.Thrist
import Data.Pass.Type

class Calculation t where
  calc :: (Eval k, Monoid b, Typeable b) => t k a b -> Calc k a b

instance Calculation Fun where
  calc = calc . pass

instance Calculation Thrist where
  calc = calc . pass

instance Calculation Pass where
  calc pass = Step (Stop ()) (\_ -> pass)

instance Calculation Calc where
  calc = id
