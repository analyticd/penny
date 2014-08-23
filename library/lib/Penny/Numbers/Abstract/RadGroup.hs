module Penny.Numbers.Abstract.RadGroup
  ( -- * Radix and grouping character types
    Period(..)
  , Comma(..)

  -- * Radix point
  , Radix
  , radPeriod
  , radComma

  -- * Grouping
  , Grouper(..)
  , Group(..)

  ) where

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

data Group r b = Group
  { grouper :: Grouper r
  , groupPayload :: b
  } deriving (Eq, Ord, Show)

instance Functor (Group a) where
  fmap f (Group g p) = Group g (f p)

radPeriod :: Radix Period
radPeriod = Radix

radComma :: Radix Comma
radComma = Radix

