module Data.Pass.Ordered where

data Ordered k a b where
  Ord b => k a b -> (Int -> (Int -> b) -> c) -> c

-- data Ordered k a b where
--  Ordered     :: Ord b => (o -> b) -> k a o -> Ordered k a b
--  OrderedAp   :: (c -> d) -> Ordered k a (b -> c) -> Ordered k a b -> Ordered k a d
--  OrderedPure :: b -> Ordered k a b
