module Penny.Numbers.Qty where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Deka.Dec (Sign(..))
import qualified Deka.Dec as D
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar

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

unpolarToQty :: Side -> Unpolar r -> Qty
unpolarToQty s = ungroupedUnpolarToQty s . ungroupUnpolar

groupedPolarToQty :: GroupedPolar r Side -> Qty
groupedPolarToQty = ungroupedPolarToQty . ungroupGroupedPolar

ungroupedPolarToQty :: UngroupedPolar r Side -> Qty
ungroupedPolarToQty = Qty . toConcrete f
  where
    f s = case s of
      Debit -> Sign0
      Credit -> Sign1

ungroupedUnpolarToQty :: Side -> UngroupedUnpolar r -> Qty
ungroupedUnpolarToQty s = ungroupedPolarToQty
  . polarizeUngroupedUnpolar s

groupedUnpolarToQty :: Side -> GroupedUnpolar r -> Qty
groupedUnpolarToQty s = ungroupedUnpolarToQty s . ungroupGroupedUnpolar

data QtyParams = QtyParams
  { qpCoeff :: Maybe (Side, NovDecs)
  , qpExponent :: Exponent
  } deriving (Eq, Ord, Show)

qtyToParams :: Qty -> QtyParams
qtyToParams (Qty c) = QtyParams mc e
  where
    Params s coe e = params c
    mc = case coe of
      CoeZero -> Nothing
      CoeNonZero nd -> Just (signToSide s, nd)

paramsToQty :: QtyParams -> Qty
paramsToQty (QtyParams mc e) = Qty . concrete $ Params s c e
  where
    (s, c) = case mc of
      Nothing -> (Sign0, CoeZero)
      Just (sd, nd) -> (sideToSign sd, CoeNonZero nd)

abstractQty :: Radix r -> Qty -> UngroupedPolar r Side
abstractQty r = fromConcrete f r . unQty
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
