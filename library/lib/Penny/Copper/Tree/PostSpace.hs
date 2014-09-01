module Penny.Copper.Tree.PostSpace where

import Penny.Copper.Tree.Spaces

-- | An item followed by at least one space.
data PostSpace a = PostSpace a Spaces
  deriving (Eq, Ord, Show)

instance Functor PostSpace where
  fmap f (PostSpace a s) = PostSpace (f a) s
