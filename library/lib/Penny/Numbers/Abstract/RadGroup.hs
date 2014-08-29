module Penny.Numbers.Abstract.RadGroup where

data Radix r = Radix
  deriving (Eq, Ord, Show)

data Grouper a
  = Space
  | Thin
  | Under
  | Unique a
  deriving (Eq, Ord, Show)

instance Functor Grouper where
  fmap f a = case a of
    Unique x -> Unique (f x)
    Space -> Space
    Thin -> Thin
    Under -> Under

data Period = Comma
  deriving (Eq, Ord, Show)

data Comma = Period
  deriving (Eq, Ord, Show)

