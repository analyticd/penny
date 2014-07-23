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
  -- ** With 'Period' as the radix point character
  , gpComma
  , gpSpace
  , gpThin
  , gpUnder

  -- * With 'Comma' as the radix point character
  , gcPeriod
  , gcSpace
  , gcThin
  , gcUnder
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

gpComma :: b -> Group Period b
gpComma = Group ','

gpSpace :: b -> Group Period b
gpSpace = Group ' '

gpThin :: b -> Group Period b
gpThin = Group '\x2009'

gpUnder :: b -> Group Period b
gpUnder = Group '_'

gcPeriod :: b -> Group Comma b
gcPeriod = Group '.'

gcSpace :: b -> Group Comma b
gcSpace = Group ' '

gcThin :: b -> Group Comma b
gcThin = Group '\x2009'

gcUnder :: b -> Group Comma b
gcUnder = Group '_'


