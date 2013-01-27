{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Monoid.Ord
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Monoid.Ord
  ( Min(..), getMin
  , Max(..), getMax
  ) where

import Control.Monad (liftM)
import Data.Monoid
import Data.Typeable
import Data.Binary

data Min a = Min a | NoMin deriving Typeable

getMin :: Num a => Min a -> a
getMin (Min a) = a
getMin NoMin = 0

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  NoMin `mappend` y = y
  x `mappend` NoMin = x
  Min x `mappend` Min y = Min (min x y)

instance Binary a => Binary (Min a) where
  put NoMin = put (0 :: Word8)
  put (Min a) = put (1 :: Word8) >> put a
  get = do
    i <- get :: Get Word8
    case i of
      0 -> return NoMin
      1 -> liftM Min get
      _ -> error "get: unknown constructor"

getMax :: Num a => Max a -> a
getMax (Max a) = a
getMax NoMax = 0

data Max a = Max a | NoMax deriving Typeable

instance Binary a => Binary (Max a) where
  put NoMax = put (0 :: Word8)
  put (Max a) = put (1 :: Word8) >> put a
  get = do
    i <- get :: Get Word8
    case i of
      0 -> return NoMax
      1 -> liftM Max get
      _ -> error "get: unknown constructor"

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  NoMax `mappend` y = y
  x `mappend` NoMax = x
  Max x `mappend` Max y = Max (max x y)
