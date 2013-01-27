{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pass.Prep
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GADTs, Rank2Types)
--
----------------------------------------------------------------------------
module Data.Pass.Prep
  ( Prep(..)
  ) where

import Data.Pass.Thrist

infixr 5 `prep`

class Prep t where
  prep :: k a b -> t k b c -> t k a c

instance Prep Thrist where
  prep x Nil       = x :- Nil
  prep x (y :- ys) = y :- prep x ys
