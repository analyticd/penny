module Penny.Lincoln.Ent where

import Data.Foldable
import Data.Traversable
import Penny.Lincoln.Commodity
import Penny.Lincoln.Qty

data Ent m = Ent Qty Commodity m
  deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f (Ent d c m) = Ent d c (f m)

instance Foldable Ent where
  foldr f z (Ent _ _ m) = f m z

instance Traversable Ent where
  sequenceA (Ent d c m) = fmap (Ent d c) m
