module Data.Pass.Env
  ( Env
  , empty
  , lookup
  , insert
  , cons
  ) where

import Control.Applicative hiding (empty)
import Prelude hiding (lookup)
import Data.Monoid (Monoid(..), (<>))
import Data.Typeable
import Data.Pass.Eval
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import GHC.Prim (Any)
import Unsafe.Coerce
import Data.Pass.Key
import Data.Pass.Thrist

newtype Id a = Id { getId :: a }

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Applicative Id where
  pure = Id
  Id a <*> Id b = Id (a b)

mapWithKey :: (k -> a -> b) -> HashMap k a -> HashMap k b
mapWithKey f m = getId (HashMap.traverseWithKey (\k a -> Id (f k a)) m)

newtype Env k a = Env (HashMap (Key k a) Any)

empty :: Env k a
empty = Env HashMap.empty

lookup :: (Eval k, Typeable b, Monoid b) => Thrist k a b -> Env k a -> Maybe b
lookup k (Env m) = unsafeCoerce <$> HashMap.lookup (Key k) m

insert :: (Eval k, Typeable b, Monoid b) => Thrist k a b -> b -> Env k a -> Env k a
insert k v (Env m) = Env $ HashMap.insert (Key k) (unsafeCoerce v) m

cons :: Eval k => a -> Env k a -> Env k a
cons a (Env m) = Env $ mapWithKey (\(Key k) old -> unsafeCoerce $ unsafeCoerce old <> eval k a) m
