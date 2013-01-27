-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Util
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Util
  ( clamp
  ) where

clamp :: Int -> Int -> Int
clamp n k
  | k <= 0    = 0
  | k >= n    = n - 1
  | otherwise = k
{-# INLINE clamp #-}
