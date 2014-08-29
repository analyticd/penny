module Penny.Numbers.Qty where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Deka.Dec (Sign(..))
import qualified Deka.Dec as D
import Deka.Native.Abstract hiding (Exponent)
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Polar
import Penny.Numbers.Abstract.Grouping

newtype Qty = Qty { unQty :: Concrete }
  deriving (Eq, Ord, Show)

data Side = Debit | Credit
  deriving (Eq, Ord, Show)

opposite :: Side -> Side
opposite s = case s of
  Debit -> Credit
  Credit -> Debit


polarToQty :: Polar r Side -> Qty
polarToQty = ungroupedPolarToQty . ungroupPolar

groupedPolarToQty :: Grouped r Side -> Qty
groupedPolarToQty = ungroupedPolarToQty . ungroupGrouped

ungroupedPolarToQty :: Ungrouped r Side -> Qty
ungroupedPolarToQty = Qty . concrete . ungroupedToParams f
  where
    f s = case s of
      Debit -> Sign0
      Credit -> Sign1

data QtyParams = QtyParams
  { qpCoeff :: Maybe (Side, NE Novem Decem)
  , qpExponent :: Exponent
  } deriving (Eq, Ord, Show)

qtyToParams :: Qty -> QtyParams
qtyToParams (Qty c) = QtyParams mc e
  where
    Params coe e = params c
    mc = case coe of
      CoeZero -> Nothing
      CoeNonZero nd s -> Just (signToSide s, nd)

paramsToQty :: QtyParams -> Qty
paramsToQty (QtyParams mc e) = Qty . concrete $ Params c e
  where
    c = case mc of
      Nothing -> CoeZero
      Just (sd, ne) -> CoeNonZero ne (sideToSign sd)

abstractQty :: Radix r -> Qty -> Ungrouped r Side
abstractQty r = paramsToUngrouped f r . params . unQty
  where
    f s = case s of
      Sign0 -> Debit
      Sign1 -> Credit

signToSide :: Sign -> Side
signToSide Sign0 = Debit
signToSide Sign1 = Credit

sideToSign :: Side -> Sign
sideToSign Debit = Sign0
sideToSign Credit = Sign1

qtySide :: Qty -> Maybe Side
qtySide (Qty c)
  | D.isZero d = Nothing
  | D.isPositive d = Just Debit
  | D.isNegative d = Just Credit
  | otherwise = error "qtyToSide: error"
  where
    d = unConcrete c

