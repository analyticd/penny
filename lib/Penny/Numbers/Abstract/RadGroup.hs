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
  , Group
  , groupPayload
  , grouper

  , comma
  , period
  , space
  , thin
  , under

  ) where

data Radix r = Radix { unRadix :: Char }
  deriving (Eq, Ord, Show)

data Group a b = Group
  { grouper :: Char
  , groupPayload :: b
  } deriving (Eq, Ord, Show)

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

comma :: b -> Group Period b
comma = Group ','

period :: b -> Group Comma b
period = Group '.'

space :: b -> Group a b
space = Group ' '

thin :: b -> Group a b
thin = Group '\x2009'

under :: b -> Group a b
under = Group '_'

