module Data.Pass.Accelerated
  ( Accelerated(..)
  ) where

import Data.Typeable
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
  largest :: k Double Double
  smallest :: k Double Double
  midrange :: k Double Double

instance Accelerated L where
  mean = robust LMean
  total = robust LTotal
  largest = robust (NthLargest 0)
  smallest = robust (NthSmallest 0)
  midrange = robust ((-1) :* NthSmallest 0 :+ NthLargest 0)

instance Accelerated k => Accelerated (Fun k) where
  mean = Fun mean
  total = Fun total
  largest = Fun largest
  smallest = Fun smallest
  midrange = Fun midrange

instance Accelerated k => Accelerated (Thrist k) where
  mean = mean :- Nil
  total = total :- Nil
  largest = largest :- Nil
  smallest = smallest :- Nil
  midrange = midrange :- Nil

instance Accelerant k => Accelerated (Calc k) where
  mean  = Stop () `Step` const meanPass
  total = Stop () `Step` const totalPass
  largest = Stop () `Step` const largestPass
  smallest = Stop () `Step` const smallestPass
  midrange = Stop () `Step` const midrangePass

instance Accelerant k => Accelerated (Pass k) where
  mean = meanPass
  total = totalPass
  largest = largestPass
  smallest = smallestPass
  midrange = midrangePass
