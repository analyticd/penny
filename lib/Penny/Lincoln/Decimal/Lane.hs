module Penny.Lincoln.Decimal.Lane where

import Deka.Native
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Rep

data Lane
  = Center
  | NonCenter Side
  -- | NonCenter (Side, Decuple)
  deriving (Eq, Ord, Show)

class Sided a where
  side :: a -> Side
  decuple :: a -> Decuple

class Laned a where
  lane :: a -> Lane

instance Laned (Rep a) where
  lane a = case a of
    RQuant q -> NonCenter . qSide $ q
    RZero _ -> Center

instance Sided (Quant a) where
  side = qSide

