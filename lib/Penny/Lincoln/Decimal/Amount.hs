module Penny.Lincoln.Decimal.Amount where

import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Digits
import Penny.Lincoln.Decimal.Lane

data Amount
  = ADigits Digits
  | AConcrete Concrete
  deriving (Eq, Ord, Show)

instance HasConcrete Amount where
  concrete r = case r of
    ADigits d -> concrete d
    AConcrete c -> concrete c

instance Laned Amount where
  lane r = case r of
    ADigits a -> lane a
    AConcrete c -> lane c
