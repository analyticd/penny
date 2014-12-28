{-# LANGUAGE FlexibleContexts #-}
-- | Number representations.
module Penny.Lincoln.Rep where

import Data.Sequence (Seq)
import Penny.Lincoln.Rep.Digits
import Penny.Lincoln.Side

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

-- | A radix point of a comma.
data RadCom = RadCom
  deriving (Eq, Ord, Show)

period :: RadCom
period = RadCom

-- | A radix point of a period.
data RadPer = RadPer
  deriving (Eq, Ord, Show)

comma :: RadPer
comma = RadPer

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

-- # Nil

data Nil r
  = NilUngrouped (NilUngrouped r)
  | NilGrouped (Maybe Zero) (Radix r) Zero (Seq Zero) r Zero (Seq Zero)
               (Seq (r, Zero, Seq Zero))
  deriving (Eq, Ord, Show)

data NilUngrouped r
  = NUZero Zero (Maybe (Radix r, Maybe (Zero, Seq Zero)))
  | NURadix (Radix r, Zero, Seq Zero)
  deriving (Eq, Ord, Show)

-- # Brim

data Brim r
  = BrimGrouped (BrimGrouped r)
  | BrimUngrouped (BrimUngrouped r)
  deriving (Eq, Ord, Show)

data BrimUngrouped r
  = BUGreaterThanOne Novem (Seq Decem) (Maybe (Radix r, Seq Decem))
  | BULessThanOne (Maybe Zero) (Radix r) (Seq Zero) Novem (Seq Decem)
  deriving (Eq, Ord, Show)

data BrimGrouped r
  = BGGreaterThanOne Novem (Seq Decem) (BG1 r)
  | BGLessThanOne (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

data BG1 r
  = BG1GroupOnLeft r Decem (Seq Decem) (Seq (r, Decem, Seq Decem))
      (Maybe (Radix r, Maybe (Decem, Seq Decem, Seq (r, Decem, Seq Decem))))
  | BG1GroupOnRight (Radix r) (Decem, Seq Decem, Seq (r, Decem, Seq Decem))
  deriving (Eq, Ord, Show)

data BG5 r
  = BG5Novem Novem (Seq Decem) r Decem (Seq Decem)
                   (Seq (r, Decem, Seq Decem))
  | BG5Zero Zero (Seq Zero) (BG6 r)
  deriving (Eq, Ord, Show)

data BG6 r
  = BG6Novem Novem (Seq Decem) r Decem (Seq Decem)
             (Seq (r, Decem, Seq Decem))
  | BG6Group r (BG7 r)
  deriving (Eq, Ord, Show)

data BG7 r
  = BG7Zeroes Zero (Seq Zero) (BG8 r)
  | BG7Novem Novem (Seq Decem) (Seq (r, Decem, Seq Decem))
  deriving (Eq, Ord, Show)

data BG8 r
  = BG8Novem Novem (Seq Decem) (Seq (r, Decem, Seq Decem))
  | BG8Group r (BG7 r)
  deriving (Eq, Ord, Show)

-- # Others

-- Same as the old Polarity

-- | Objects that can be neutral or, alterntively, can be polar; for
-- example, numbers on a number line that includes zero.
data CenterOrOffCenter n o p
  = Center n
  -- ^ This object is neutral.

  | OffCenter o p
  -- ^ This object is not neutral.  The second field indicates the
  -- polarity of the object, while the first is the object itself.
  deriving (Eq, Ord, Show)

changeOffCenterType
  :: p'
  -> CenterOrOffCenter n o p
  -> Maybe (CenterOrOffCenter n o p')
changeOffCenterType _ (Center _) = Nothing
changeOffCenterType p' (OffCenter o _) = Just $ OffCenter o p'

-- Same as old Stokely

-- | Number representations that may be neutral or, alternatively, may
-- be non-neutral.  The first type variable is the type of the radix
-- point and grouping character; see, for example,
-- "Penny.Core.Anna.RadCom" or "Penny.Core.Anna.RadPer".  The second
-- type variable is the polarity; see, for example, "Penny.Core.Side"
-- or "Penny.Core.PluMin".

newtype NilOrBrim r p
  = NilOrBrim (CenterOrOffCenter (Nil r) (Brim r) p)
  deriving (Eq, Ord, Show)

-- Same as old Walker

-- | Qty representations that may be neutral or non-neutral.  The
-- type variable is the type of the radix point and grouping
-- character; see, for example, 'Penny.Core.Anna.RadCom.T' or
-- 'Penny.Core.Anna.RadPer.T'.

newtype QtyNeutralOrNonNeutral r
  = QtyNeutralOrNonNeutral (NilOrBrim r Side)
  deriving (Eq, Ord, Show)

-- Same as old Muddy

-- | Qty representations that may be neutral or non-neutral and have a
-- radix that is either a period or a comma.

newtype QtyNeutralOrNonNeutralAnyRadix
  = QtyNeutralOrNonNeutralAnyRadix
    (Either (QtyNeutralOrNonNeutral RadCom)
            (QtyNeutralOrNonNeutral RadPer))
  deriving (Eq, Ord, Show)

-- Same as old Philly

-- | Qty representations that are non-neutral and have a radix that is
-- either a period or a comma.

newtype QtyNonNeutralAnyRadix
  = QtyNonNeutralAnyRadix
    (Either (Brim RadCom) (Brim RadPer))
    deriving (Eq, Ord, Show)
