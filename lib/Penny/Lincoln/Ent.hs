module Penny.Lincoln.Ent where

import Penny.Lincoln.Amount
import Data.Foldable
import Data.Traversable

data Ent m = Ent Amount m
  deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f (Ent a m) = Ent a (f m)

instance Foldable Ent where
  foldr f z (Ent _ m) = f m z

instance Traversable Ent where
  sequenceA (Ent a m) = fmap (Ent a) m
