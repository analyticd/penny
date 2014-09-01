module Penny.Copper.Tree.PreSpace where

import Penny.Copper.Tree.Spaces

data PreSpace a = PreSpace Spaces a
  deriving (Eq, Ord, Show)

instance Functor PreSpace where
  fmap f (PreSpace s a) = PreSpace s (f a)
