module Data.Pass.Named
  ( Named(..)
  ) where

class Named k where
  showsFun         :: Int -> k a b -> String -> String
