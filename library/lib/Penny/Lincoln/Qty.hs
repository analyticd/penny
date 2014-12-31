module Penny.Lincoln.Qty where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Natural
import Penny.Lincoln.Side
import Penny.Lincoln.Rep
import Penny.Lincoln.NonZero
import Penny.Lincoln.Offset

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

qtySide :: Qty -> Maybe Side
qtySide (Qty (Decimal sig _))
  | sig < 0 = Just Debit
  | sig > 0 = Just Credit
  | otherwise = Nothing

newtype QtyNonZero = QtyNonZero DecNonZero
  deriving (Eq, Ord, Show)

instance HasOffset QtyNonZero where
  offset (QtyNonZero dnz) = QtyNonZero (offset dnz)

qtyNonZeroToQty :: QtyNonZero -> Qty
qtyNonZeroToQty (QtyNonZero dnz) = Qty (decNonZeroToDecimal dnz)

qtyToQtyNonZero :: Qty -> Maybe QtyNonZero
qtyToQtyNonZero (Qty d) = fmap QtyNonZero $ decimalToDecNonZero d

qtyNonZeroSide :: QtyNonZero -> Side
qtyNonZeroSide (QtyNonZero (DecNonZero nz _))
  | i < 0 = Debit
  | otherwise = Credit
  where
    i = nonZeroToInteger nz

newtype QtyUnsigned = QtyUnsigned DecUnsigned
  deriving (Eq, Ord, Show)

qtyUnsignedToQtyWithSide :: Side -> QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyWithSide s (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Nothing
  | otherwise = Just
      (Qty (Decimal (addSign s . naturalToInteger $ sig) expt))

addSign :: Num a => Side -> a -> a
addSign Debit = negate
addSign Credit = id

qtyUnsignedToQtyNoSide :: QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyNoSide (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Just
      (Qty (Decimal 0 expt))
  | otherwise = Nothing

class HasQty a where
  toQty :: a -> Qty

instance HasQty QtyNonZero where
  toQty (QtyNonZero (DecNonZero sig expt)) =
    Qty $ Decimal (nonZeroToInteger sig) expt

decPositiveToQty :: Side -> DecPositive -> Qty
decPositiveToQty s (DecPositive pos expt)
  = Qty $ Decimal (addSign s . naturalToInteger $ pos) expt

instance HasQty (QtyRep a) where
  toQty (QtyRep (NilOrBrimPolar cof)) = case cof of
    Center nil -> Qty . Decimal 0 . toExponent $ nil
    OffCenter brim s -> Qty . addSign s . toDecimal
      . toDecPositive $ brim

instance HasQty QtyRepAnyRadix where
  toQty (QtyRepAnyRadix ei) = case ei of
    Left q -> toQty q
    Right q -> toQty q

