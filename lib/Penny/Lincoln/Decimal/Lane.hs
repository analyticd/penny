{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Lincoln.Decimal.Lane where

import Deka.Native
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Components

-- | Represents whether something is 'Debit', 'Credit', or neither
-- (which is a 'Center').
data Lane a
  = Center

  | NonCenter (a, Decuple)
  -- ^ Anything that is a 'Debit' or a 'Credit' must also have a
  -- non-zero coefficient, which is the 'Decuple'.

  deriving (Eq, Ord, Show)

class HasDecuple a => Sided a where
  side :: a -> Side

class Laned a b where
  lane :: a -> Lane b

