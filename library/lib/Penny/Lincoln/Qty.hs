module Penny.Lincoln.Qty where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Natural
import Penny.Lincoln.Side

newtype Qty = Qty Decimal
  deriving (Eq, Ord, Show)

instance Num Qty where
  Qty x + Qty y = Qty $ x + y
  Qty x - Qty y = Qty $ x - y
  Qty x * Qty y = Qty $ x * y
  negate (Qty x) = Qty (negate x)
  abs (Qty x) = Qty (abs x)
  signum (Qty x) = Qty (signum x)
  fromInteger i = Qty (fromInteger i)

side :: Qty -> Maybe Side
side (Qty (Decimal sig _))
  | sig < 0 = Just Debit
  | sig > 0 = Just Credit
  | otherwise = Nothing

newtype QtyUnsigned = QtyUnsigned DecUnsigned
  deriving (Eq, Ord, Show)

qtyUnsignedToQtyWithSide :: Side -> QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyWithSide s (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Nothing
  | otherwise = Just
      (Qty (Decimal (addSign . naturalToInteger $ sig) expt))
  where
    addSign = case s of
      Debit -> negate
      Credit -> id

qtyUnsignedToQtyNoSide :: QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyNoSide (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Just
      (Qty (Decimal 0 expt))
  | otherwise = Nothing
