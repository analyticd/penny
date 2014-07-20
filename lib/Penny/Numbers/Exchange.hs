module Penny.Numbers.Exchange where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Deka.Dec (Sign(..))
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates

newtype Exch = Exch { unExch :: Concrete }
  deriving (Eq, Ord, Show)

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

concreteExch :: Ungrouped PluMin r -> Exch
concreteExch = Exch . toConcrete f
  where
    f s = case s of
      Plus -> Sign0
      Minus -> Sign1

abstractExch :: Radix r -> Exch -> Ungrouped PluMin r
abstractExch r = fromConcrete f r . unExch
  where
    f s = case s of
      Sign0 -> Plus
      Sign1 -> Minus
