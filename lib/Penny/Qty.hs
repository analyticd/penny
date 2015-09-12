{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Quantities.  A quantity is a signed decimal number; however, it
-- always represents quantities that may be a debit or a credit (as
-- opposed to prices, which do not have a debit or credit.)
--
-- Unlike the types in "Penny.Representation", you can perform arithmetic
-- on these types.  Unlike the types in "Penny.Representation", these
-- types may not be \"represented\"--that is, displayed to the user in
-- a friendly way.  To display a 'Qty', convert it to a representation
-- type using one of the functions in this module.
module Penny.Qty
  ( -- * Qty
    Qty(..)
  , HasQty(..)
  , decPositiveToQty

  -- * Non-zero Qty
  , QtyNonZero(..)
  , displayQtyNonZero
  , HasQtyNonZero(..)
  , qtyToQtyNonZero

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

import Penny.Decimal
import Penny.Natural
import Penny.Side
import Penny.Representation
import Penny.NonZero
import Penny.Offset
import Penny.PluMin
import Penny.Display

-- | A quantity.  Can be a debit, credit, or zero.
newtype Qty = Qty Decimal
  deriving Show

-- | Anything that has, or can be converted to, a 'Qty'.
class HasQty a where
  toQty :: a -> Qty

instance (HasQty a, HasQty b) => HasQty (Either a b) where
  toQty = either toQty toQty

instance HasQty Qty where toQty = id

instance Num Qty where
  Qty x + Qty y = Qty $ x + y
  Qty x - Qty y = Qty $ x - y
  Qty x * Qty y = Qty $ x * y
  negate (Qty x) = Qty (negate x)
  abs (Qty x) = Qty (abs x)
  signum (Qty x) = Qty (signum x)
  fromInteger i = Qty (fromInteger i)

instance SidedOrNeutral Qty where
  sideOrNeutral (Qty (Exponential sig _))
    | sig < 0 = Just Debit
    | sig > 0 = Just Credit
    | otherwise = Nothing

-- | Quantities that are always a debit or a credit; never zero.
newtype QtyNonZero = QtyNonZero DecNonZero
  deriving Show

-- | Displays a QtyNonZero using a period radix and no grouping.
displayQtyNonZero :: QtyNonZero -> String
displayQtyNonZero qnz =  case stripDecimalSign . (\(Qty d) -> d)
  . toQty $ qnz of
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

instance HasQty QtyNonZero where
  toQty (QtyNonZero dnz) = Qty (decNonZeroToDecimal dnz)

qtyToQtyNonZero :: Qty -> Maybe QtyNonZero
qtyToQtyNonZero (Qty d) = fmap QtyNonZero $ decimalToDecNonZero d

instance HasSide QtyNonZero where
  side (QtyNonZero (Exponential nz _))
    | i < 0 = Debit
    | otherwise = Credit
    where
      i = nonZeroToInteger nz

-- | A quantity that is neither a debit nor a credit.  However, it may
-- be zero.
newtype QtyUnsigned = QtyUnsigned DecUnsigned
  deriving Show

class HasQtyUnsigned a where
  toQtyUnsigned :: a -> QtyUnsigned

qtyUnsignedToQtyWithSide :: Side -> QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyWithSide s (QtyUnsigned (Exponential sig expt))
  | naturalToInteger sig == 0 = Nothing
  | otherwise = Just
      (Qty (Exponential (addSideSign s . naturalToInteger $ sig) expt))

addSideSign :: Num a => Side -> a -> a
addSideSign Debit = negate
addSideSign Credit = id

qtyUnsignedToQtyNoSide :: QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyNoSide (QtyUnsigned (Exponential sig expt))
  | naturalToInteger sig == 0 = Just
      (Qty (Exponential 0 expt))
  | otherwise = Nothing

decPositiveToQty :: Side -> DecPositive -> Qty
decPositiveToQty s (Exponential pos expt)
  = Qty $ Exponential (addSideSign s . naturalToInteger $ pos) expt

instance (HasExponent n, HasDecPositive o)
  => HasQty (CenterOrOffCenter n o Side) where
  toQty cof = case cof of
    Center nil -> Qty . Exponential 0 . toExponent $ nil
    OffCenter brim s -> Qty . addSideSign s . toDecimal
      . toDecPositive $ brim

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
repQty ei q = eiq
  where
    eiq = case ei of
      Left Nothing -> Left
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Right Nothing -> Right
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Left (Just grp) -> Left $
        case repUngroupedQty Radix q of
          Center nu -> Center . NilU $ nu
          OffCenter bu s -> case groupBrimUngrouped grp bu of
            Nothing -> OffCenter (BrimUngrouped bu) s
            Just grpd -> OffCenter (BrimGrouped grpd) s
      Right (Just grp) -> Right $
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

