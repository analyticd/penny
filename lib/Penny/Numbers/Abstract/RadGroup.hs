module Penny.Numbers.Abstract.RadGroup
  ( -- * Radix and grouping character types
    Period
  , Comma
  , Thin
  , Space
  , Under

  -- * Radix point
  , Radix
  , unRadix
  , radPeriod
  , radComma

  -- * Grouping
  , Group
  , groupPayload
  , grouper
  , groupPeriod
  , groupComma
  , groupThin
  , groupSpace
  , groupUnder
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

groupPeriod :: b -> Group Period b
groupPeriod = Group '.'

groupComma :: b -> Group Comma b
groupComma = Group ','

groupThin :: b -> Group Thin b
groupThin = Group '\x2009'

groupSpace :: b -> Group Space b
groupSpace = Group ' '

groupUnder :: b -> Group Under b
groupUnder = Group '_'


