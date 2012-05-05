module Data.Pass.Util
  ( clamp
  ) where

clamp :: Int -> Int -> Int
clamp n k
  | k <= 0    = 0
  | k >= n    = n - 1
  | otherwise = k
{-# INLINE clamp #-}
