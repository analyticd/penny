module Penny.Tree.PostSpace where

import qualified Penny.Tree.Spaces as Spaces

data T a = T
  { payload :: a
  , spaces :: Spaces.T
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T p s) = T (f p) s
