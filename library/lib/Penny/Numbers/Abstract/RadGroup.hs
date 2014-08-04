module Penny.Numbers.Abstract.RadGroup
  ( -- * Radix and grouping character types
    Period
  , Comma

  -- * Radix point
  , Radix
  , unRadix
  , radPeriod
  , radComma

  -- * Grouping
  , Grouper
  , unGrouper
  , Group(..)

  , comma
  , period
  , space
  , thin
  , under

  ) where

data Radix r = Radix { unRadix :: Char }
  deriving (Eq, Ord, Show)

data Grouper r = Grouper { unGrouper :: Char }
  deriving (Eq, Ord, Show)

data Group r b = Group
  { grouper :: Grouper r
  , groupPayload :: b
  } deriving (Eq, Ord, Show)

instance Functor (Group a) where
  fmap f (Group g p) = Group g (f p)

data Period = Period
  deriving (Eq, Ord, Show)

data Comma = Comma
  deriving (Eq, Ord, Show)

data Thin = Thin
  deriving (Eq, Ord, Show)

data Space = Space
  deriving (Eq, Ord, Show)

data Under = Under
  deriving (Eq, Ord, Show)

radPeriod :: Radix Period
radPeriod = Radix '.'

radComma :: Radix Comma
radComma = Radix ','

comma :: Grouper Period
comma = Grouper ','

period :: Grouper Comma
period = Grouper '.'

space :: Grouper a
space = Grouper ' '

thin :: Grouper a
thin = Grouper '\x2009'

under :: Grouper a
under = Grouper '_'

