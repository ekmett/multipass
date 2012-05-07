module Data.Pass.Step
  ( Step(..)
  ) where

import Data.Pass.Type
import Data.Pass.Calc
import Data.Pass.Prep

class Prep t => Step t where
  step :: Pass k a b -> t k a b

instance Step Pass where
  step = id

instance Step Calc where
  step k = k :& Stop
