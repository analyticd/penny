{-# LANGUAGE FlexibleContexts #-}

-- | Number representations.
--
-- These types contain a context-free grammar for number
-- representations.  A number representation is not suitable for
-- arithmetic; however, it \"remembers\" exactly how the user entered
-- a number, complete with grouping characters and the radix point
-- (which may be a period or a comma.)
module Penny.Lincoln.Rep
  ( -- * Radix and grouping
    Radix(..)
  , Grouper(..)
  , RadCom(..)
  , RadPer(..)

  -- * Zero
  , Zero(..)

  -- * Nil representations
  -- | These represent numbers whose significand is zero.
  , Nil(..)
  , NilGrouped(..)
  , NilUngrouped(..)

  -- * Brim representations
  --
  -- | These represent numbers whose significand is other than zero.
  , Brim(..)
  , BrimGrouped(..)
  , BrimUngrouped(..)
  , BG1(..)
  , BG5(..)
  , BG6(..)
  , BG7(..)
  , BG8(..)

  -- * Aggregate types
  -- | These types aggregate other types.
  , CenterOrOffCenter(..)
  , changeOffCenterType
  , NilOrBrimPolar(..)
  , NilOrBrimScalar(..)
  , NilOrBrimScalarAnyRadix(..)
  , nilOrBrimScalarAnyRadixToQty
  , RepNonNeutralNoSide(..)

  -- * Qty types
  --
  -- | These types represent quantities (as opposed to prices).
  , QtyRep(..)
  , QtyRepAnyRadix(..)

  -- * Exch types
  --
  -- | These types represent exchanges.
  , ExchRep(..)
  , ExchRepAnyRadix(..)
  ) where

import Data.Sequence (Seq)
import Penny.Lincoln.Rep.Digits
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

data Grouper
  = ThinSpace
  | Underscore
  deriving (Eq, Ord, Show)

-- | A radix point of a comma.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a comma.  When used alone, it represents a grouping
-- character, which may be a period or other grouping character.
data RadCom
  = Period
  -- ^ When used as a grouping character, a RadCom can be a period
  | RCGrouper Grouper
  -- ^ When used as a grouping character, a RadCom can also be a
  -- 'ThinSpace' or an 'Underscore'.
  deriving (Eq, Ord, Show)

-- | A radix point of a period.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a period.  When used alone, it represents a grouping
-- character, which may be a comma or other grouping character.
-- | A radix point of a period.
data RadPer
  = Comma
  -- ^ When used as a grouping character, a RadPer can be a comma
  | RPGrouper Grouper
  -- ^ When used as a grouping character, a RadPer can also be a
  -- 'ThinSpace' or an 'Underscore'.

  deriving (Eq, Ord, Show)

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

-- # Nil

data Nil r
  = NilU (NilUngrouped r)
  | NilG (NilGrouped r)
  deriving (Eq, Ord, Show)

data NilGrouped r
  = NilGrouped (Maybe Zero) (Radix r)
               Zero (Seq Zero) r Zero (Seq Zero)
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
  | BG1GroupOnRight (Radix r) Decem (Seq Decem) (Seq (r, Decem, Seq Decem))
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

-- | Number representations that may be neutral or non-neutral.  The
-- type variable is the type of the radix point and grouping
-- character.  Unlike 'NilOrBrimPolar', a 'NilOrBrimScalar' does not
-- have a polarity.

newtype NilOrBrimScalar r
  = NilOrBrimScalar (Either (Nil r) (Brim r))
  deriving (Eq, Ord, Show)

-- | Number types that may be neutral or non-neutral, with either a
-- comma or period radix.  Does not have a polarity.
newtype NilOrBrimScalarAnyRadix
  = NilOrBrimScalarAnyRadix (Either (NilOrBrimScalar RadCom)
                                    (NilOrBrimScalar RadPer))
  deriving (Eq, Ord, Show)

-- | Adds polarity to a 'NilOrBrimScalarAnyRadix' to transform it to a
-- 'QtyRepAnyRadix'.  Nil values do not receive a polarity; all other
-- values are assigned to the given 'Side'.
nilOrBrimScalarAnyRadixToQty
  :: Side
  -- ^ Assign this 'Side' to polar values
  -> NilOrBrimScalarAnyRadix
  -> QtyRepAnyRadix
nilOrBrimScalarAnyRadixToQty s (NilOrBrimScalarAnyRadix e) =
  QtyRepAnyRadix e'
  where
    e' = case e of
      Left (NilOrBrimScalar (Left nilCom)) ->
        Left (QtyRep (NilOrBrimPolar (Center nilCom)))
      Left (NilOrBrimScalar (Right brimCom)) ->
        Left (QtyRep (NilOrBrimPolar (OffCenter brimCom s)))
      Right (NilOrBrimScalar (Left nilPer)) ->
        Right (QtyRep (NilOrBrimPolar (Center nilPer)))
      Right (NilOrBrimScalar (Right brimPer)) ->
        Right (QtyRep (NilOrBrimPolar (OffCenter brimPer s)))

-- Same as old Stokely

-- | Number representations that may be neutral or, alternatively, may
-- be non-neutral.  The first type variable is the type of the radix
-- point and grouping character; see, for example, 'RadCom' or
-- 'RadPer'.  The second type variable is the polarity; see, for
-- example, 'Penny.Lincoln.Side.Side'.

newtype NilOrBrimPolar r p
  = NilOrBrimPolar (CenterOrOffCenter (Nil r) (Brim r) p)
  deriving (Eq, Ord, Show)

-- Same as old Philly

-- | Representations that are non-neutral and have a radix that is
-- either a period or a comma.  Though they are non-neutral, they do
-- not have a side.

newtype RepNonNeutralNoSide
  = RepNonNeutralNoSide
    (Either (Brim RadCom) (Brim RadPer))
    deriving (Eq, Ord, Show)

-- # Qty representations

-- Same as old Walker

-- | Qty representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'Side'.
--
-- This is a complete representation of a quantity; that is, it can
-- represent any quantity.

newtype QtyRep r
  = QtyRep (NilOrBrimPolar r Side)
  deriving (Eq, Ord, Show)

-- Same as old Muddy

-- | Qty representations that may be neutral or non-neutral and have a
-- radix that is either a period or a comma.  If non-neutral, also
-- contains a 'Side'.

newtype QtyRepAnyRadix
  = QtyRepAnyRadix
    (Either (QtyRep RadCom)
            (QtyRep RadPer))
  deriving (Eq, Ord, Show)

-- # Exch representations

-- | Exch representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'PluMin'.
--
-- This is a complete representation of an 'Penny.Lincoln.Exch.Exch';
-- that is, it can represent any 'Penny.Lincoln.Exch.Exch'.

newtype ExchRep r = ExchRep (NilOrBrimPolar r PluMin)
  deriving (Eq, Ord, Show)

-- | Exch representations that may have a radix of 'RadCom' or
-- 'RadPer' and may be neutral or non-neutral.  If non-neutral, also
-- contains a 'PluMin'.

newtype ExchRepAnyRadix = ExchRepAnyRadix
  (Either (ExchRep RadCom) (ExchRep RadPer))
  deriving (Eq, Ord, Show)
