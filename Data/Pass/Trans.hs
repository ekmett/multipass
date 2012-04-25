module Data.Pass.Trans
  ( Trans(..)
  ) where

import Data.Typeable
import Data.Monoid
-- import Data.Pass.Eval

class Trans t where
  trans :: (Monoid b, Typeable b) => k a b -> t k a b
