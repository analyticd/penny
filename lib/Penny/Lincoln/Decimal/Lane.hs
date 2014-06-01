module Penny.Lincoln.Decimal.Lane where

import Deka.Native

data Side
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

data Lane
  = Center
  | NonCenter (Side, Decuple)
  deriving (Eq, Ord, Show)

class Sided a where
  side :: a -> Side
  decuple :: a -> Decuple

class Laned a where
  lane :: a -> Lane
