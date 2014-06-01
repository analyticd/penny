module Penny.Lincoln.Decimal.Amount where

import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Rep
import Penny.Lincoln.Decimal.Lane

data Amount a
  = ARep (Rep a)
  | AConcrete Concrete
  deriving Show

instance Functor Amount where
  fmap f r = case r of
    ARep a -> ARep (fmap f a)
    AConcrete c -> AConcrete c

instance HasConcrete (Amount a) where
  concrete r = case r of
    ARep a -> concrete a
    AConcrete c -> c

instance Laned (Amount a) where
  lane r = case r of
    ARep a -> lane a
    AConcrete c -> lane c
