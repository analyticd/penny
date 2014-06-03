module Penny.Lincoln.Decimal.Amount where

import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Lane

data Amount
  = AAbstract Abstract
  | AConcrete Concrete
  deriving (Eq, Ord, Show)

instance HasConcrete Amount where
  concrete r = case r of
    AAbstract d -> concrete d
    AConcrete c -> concrete c

instance Laned Amount where
  lane r = case r of
    AAbstract a -> lane a
    AConcrete c -> lane c
