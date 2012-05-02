module Data.Pass.Accelerated
  ( Accelerated(..)
  ) where

import Data.Pass.Type
import Data.Pass.Calc
import Data.Pass.L
import Data.Pass.Fun
import Data.Pass.Thrist
import Data.Pass.Robust
import Data.Pass.Accelerant

class Accelerated k where
  mean :: k Double Double
  total :: k Double Double

instance Accelerated L where
  mean = robust LMean
  total = robust LTotal

instance Accelerated k => Accelerated (Fun k) where
  mean = Fun mean
  total = Fun total

instance Accelerated k => Accelerated (Thrist k) where
  mean = mean :- Nil
  total = total :- Nil

instance Accelerant k => Accelerated (Calc k) where
  mean  = Stop () `Step` const meanPass
  total = Stop () `Step` const totalPass

instance Accelerant k => Accelerated (Pass k) where
  mean = meanPass
  total = totalPass
