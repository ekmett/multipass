module Data.Pass.Call
  ( Call(..)
  ) where

import Data.Pass.Named

class Named k => Call k where
  call :: k a b -> a -> b
