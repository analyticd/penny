module Penny.Core.Grouper where

data T a
  = Space
  | Thin
  | Under
  | Unique a
  deriving (Eq, Ord, Show)

instance Functor T where
  fmap f t = case t of
    Unique a -> Unique (f a)
    Space -> Space
    Thin -> Thin
    Under -> Under
