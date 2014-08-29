module Penny.Numbers.Exchange where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Deka.Dec (Sign(..))
import Penny.Numbers.Abstract.RadGroup

newtype Exch = Exch { unExch :: Concrete }
  deriving (Eq, Ord, Show)

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

{-
concreteExch :: UngroupedPolar r PluMin -> Exch
concreteExch = Exch . toConcrete f
  where
    f s = case s of
      Plus -> Sign0
      Minus -> Sign1

abstractExch :: Radix r -> Exch -> UngroupedPolar r PluMin
abstractExch r = fromConcrete f r . unExch
  where
    f s = case s of
      Sign0 -> Plus
      Sign1 -> Minus
-}
