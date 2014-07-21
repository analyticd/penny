module Penny.Numbers.Qty where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Deka.Dec (Sign(..))
import qualified Deka.Dec as D
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates

newtype Qty = Qty { unQty :: Concrete }
  deriving (Eq, Ord, Show)

data Side = Debit | Credit
  deriving (Eq, Ord, Show)

opposite :: Side -> Side
opposite s = case s of
  Debit -> Credit
  Credit -> Debit

concreteQty :: Ungrouped Side r -> Qty
concreteQty = Qty . toConcrete f
  where
    f s = case s of
      Debit -> Sign0
      Credit -> Sign1

qtySide :: Qty -> Maybe Side
qtySide (Qty c)
  | D.isZero d = Nothing
  | D.isPositive d = Just Debit
  | D.isNegative d = Just Credit
  | otherwise = error "qtySide: bad concrete"
  where
    d = unConcrete c

abstractQty :: Radix r -> Qty -> Ungrouped Side r
abstractQty r = fromConcrete f r . unQty
  where
    f s = case s of
      Sign0 -> Debit
      Sign1 -> Credit
