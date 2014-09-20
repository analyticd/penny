module Penny.Tree.PreSpace where

import qualified Penny.Tree.Spaces as Spaces

data T a = T
  { spaces :: Spaces.T
  , payload :: a
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T s a) = T s (f a)
