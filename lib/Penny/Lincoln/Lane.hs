module Penny.Lincoln.Lane where

data Side
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

data Lane
  = Center
  | NonCenter Side
  deriving (Eq, Ord, Show)

class Sided a where
  side :: a -> Side

class Laned a where
  lane :: a -> Lane
