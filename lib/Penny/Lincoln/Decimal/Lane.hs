module Penny.Lincoln.Decimal.Lane where

import Deka.Native
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Components

data Lane
  = Center
  | NonCenter (Side, Decuple)
  deriving (Eq, Ord, Show)

class HasDecuple a => Sided a where
  side :: a -> Side

class Laned a where
  lane :: a -> Lane

