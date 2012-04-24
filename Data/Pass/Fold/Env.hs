{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Data.Pass.Fold.Env
  ( Env
  , empty
  , lookup
  , insert
  , add
  ) where

import Prelude hiding (lookup)
import Data.Functor
import Data.Monoid (Monoid(..), (<>))
import Data.Typeable
import Data.Pass.Eval
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet
import Data.HashSet (HashSet)
import GHC.Prim (Any)
import Unsafe.Coerce
import Data.Pass.Env.FoldKey

-- * Used to remember the result of a computation
newtype Env k a = Env (HashMap (FoldKey k a) Any)

empty :: Env k a
empty = Env HashMap.empty

lookup :: (Eval k, Typeable b) => k a b -> Env k a -> Maybe b
lookup k (Env m) = unsafeCoerce <$> HashMap.lookup (FoldKey k) m

insert :: (Eval k, Typeable b) => k a b -> b -> Env k a -> Env k a
insert k v (Env m) = Env $ HashMap.insert (FoldKey k) (unsafeCoerce v) m

(@#) :: Eval k => Fold k a b -> [a] -> b
