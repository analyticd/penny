-- | Quantities.  A quantity is a signed decimal number; however, it
-- always represents quantities that may be a debit or a credit (as
-- opposed to prices, which do not have a debit or credit.)
--
-- Unlike the types in "Penny.Lincoln.Rep", you can perform arithmetic
-- on these types.  Unlike the types in "Penny.Lincoln.Rep", these
-- types may not be \"represented\"--that is, displayed to the user in
-- a friendly way.  To display a 'Qty', convert it to a representation
-- type using one of the functions in this module.
module Penny.Lincoln.Qty
  ( -- * Qty
    Qty(..)
  , qtySide
  , HasQty(..)
  , decPositiveToQty

  -- * Non-zero Qty
  , QtyNonZero(..)
  , displayQtyNonZero
  , HasQtyNonZero(..)
  , qtyNonZeroToQty
  , qtyToQtyNonZero
  , qtyNonZeroSide

  -- * Unsigned Qty
  , QtyUnsigned(..)
  , HasQtyUnsigned(..)
  , qtyUnsignedToQtyWithSide
  , addSideSign
  , qtyUnsignedToQtyNoSide

  -- * Representations
  , repUngroupedQty
  , repUngroupedQtyNonZero
  , repUngroupedQtyUnsigned
  , repQty
  ) where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Natural
import Penny.Lincoln.Side
import Penny.Lincoln.Rep
import Penny.Lincoln.NonZero
import Penny.Lincoln.Offset
import Penny.Lincoln.PluMin

newtype Qty = Qty Decimal
  deriving (Eq, Ord, Show)

class HasQty a where
  toQty :: a -> Qty

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

-- | Displays a QtyNonZero using a period radix and no grouping.
displayQtyNonZero :: QtyNonZero -> String
displayQtyNonZero qnz =  case stripDecimalSign . (\(Qty d) -> d)
  . qtyNonZeroToQty $ qnz of
  Left dz -> display (repUngroupedDecZero rdx dz) ""
  Right (dp, pm) -> display (conv pm) . (' ':)
    . display (repUngroupedDecPositive rdx dp) $ ""
    where
      conv = fromSign :: PluMin -> Side
  where
    rdx = Radix :: Radix RadPer

class HasQtyNonZero a where
  toQtyNonZero :: a -> QtyNonZero

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

class HasQtyUnsigned a where
  toQtyUnsigned :: a -> QtyUnsigned

qtyUnsignedToQtyWithSide :: Side -> QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyWithSide s (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Nothing
  | otherwise = Just
      (Qty (Decimal (addSideSign s . naturalToInteger $ sig) expt))

addSideSign :: Num a => Side -> a -> a
addSideSign Debit = negate
addSideSign Credit = id

qtyUnsignedToQtyNoSide :: QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyNoSide (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Just
      (Qty (Decimal 0 expt))
  | otherwise = Nothing

instance HasQty QtyNonZero where
  toQty (QtyNonZero (DecNonZero sig expt)) =
    Qty $ Decimal (nonZeroToInteger sig) expt

decPositiveToQty :: Side -> DecPositive -> Qty
decPositiveToQty s (DecPositive pos expt)
  = Qty $ Decimal (addSideSign s . naturalToInteger $ pos) expt

instance HasQty (QtyRep a) where
  toQty (QtyRep (NilOrBrimPolar cof)) = case cof of
    Center nil -> Qty . Decimal 0 . toExponent $ nil
    OffCenter brim s -> Qty . addSideSign s . toDecimal
      . toDecPositive $ brim

instance HasQty QtyRepAnyRadix where
  toQty (QtyRepAnyRadix ei) = case ei of
    Left q -> toQty q
    Right q -> toQty q

-- # Representing

-- | Represents a 'Qty', with optional grouping.
repQty
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Determines which radix is used.  If you also supply a grouping
  -- character, 'repQty' will try to group the 'Qty' as well.
  -- Grouping will fail if the absolute value of the 'Qty' is less
  -- than @10000@.  In that case the 'Qty' will be represented without
  -- grouping.
  -> Qty
  -> QtyRepAnyRadix
repQty ei q = QtyRepAnyRadix eiq
  where
    eiq = case ei of
      Left Nothing -> Left . QtyRep . NilOrBrimPolar
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Right Nothing -> Right . QtyRep . NilOrBrimPolar
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Left (Just grp) -> Left . QtyRep . NilOrBrimPolar $
        case repUngroupedQty Radix q of
          Center nu -> Center . NilU $ nu
          OffCenter bu s -> case groupBrimUngrouped grp bu of
            Nothing -> OffCenter (BrimUngrouped bu) s
            Just grpd -> OffCenter (BrimGrouped grpd) s
      Right (Just grp) -> Right . QtyRep . NilOrBrimPolar $
        case repUngroupedQty Radix q of
          Center nu -> Center . NilU $ nu
          OffCenter bu s -> case groupBrimUngrouped grp bu of
            Nothing -> OffCenter (BrimUngrouped bu) s
            Just grpd -> OffCenter (BrimGrouped grpd) s


repUngroupedQty
  :: Radix r
  -> Qty
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) Side
repUngroupedQty rdx (Qty dec) = case repUngroupedDecimal rdx dec of
  Center n -> Center n
  OffCenter o p -> OffCenter o (fromSign p)

repUngroupedQtyNonZero
  :: Radix r
  -> QtyNonZero
  -> (BrimUngrouped r, Side)
repUngroupedQtyNonZero rdx (QtyNonZero qnz) = (bu, side)
  where
    (bu, sgn) = repUngroupedDecNonZero rdx qnz
    side = fromSign sgn

repUngroupedQtyUnsigned
  :: Radix r
  -> QtyUnsigned
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) ()
repUngroupedQtyUnsigned rdx (QtyUnsigned qu) = repUngroupedDecUnsigned rdx qu
