module Penny.Numbers.Abstract.Aggregates
  ( -- * Polarity
    Polarity(..)

    -- * Ungrouped - low-level aggregates
  , UngroupedZero(..)
  , UngroupedNonZero(..)

  -- * Ungrouped - polar and unpolar
  , UngroupedUnpolar(..)
  , UngroupedPolar(..)
  , neutralizeUngrouped
  , polarizeUngrouped

  -- * Grouped - low-level aggregates

  -- | There is no need for a GroupedZero group, as there is only one
  -- type of grouped zero.
  , GroupedNonZero(..)

  -- * Grouped - polar and unpolar
  , GroupedUnpolar(..)
  , GroupedPolar(..)
  , neutralizeGrouped
  , polarizeGrouped

  -- * All unpolar and polar
  , Unpolar(..)
  , Polar(..)
  , neutralize
  , polarize

  -- * All abstract types, polar and unpolar
  , Abstract(..)

  ) where

import Data.Sums
import Penny.Numbers.Abstract.Unpolar

-- # Polarity

-- | Holds polar types, along with their polarity.  The first type
-- variable is the type for neutral types.  The second type variable
-- is for opposing types.  The third type variable is the polarity
-- itself, e.g. Debit or Credit.
data Polarity n o p
  = Center n
  | OffCenter p o
  deriving (Eq, Ord, Show)

-- # Ungrouped

newtype UngroupedZero r
  = UngroupedZero { unUngroupedZero :: S2 UZBare (UZTrailing r) }
  deriving (Eq, Ord, Show)

newtype UngroupedNonZero r
  = UngroupedNonZero
  { unUngroupedNonZero :: S3 UNWhole (UNWholeRadix r) (UNRadFrac r) }
  deriving (Eq, Ord, Show)

-- ## Ungrouped - polar and unpolar

newtype UngroupedUnpolar r
  = UngroupedUnpolar
  { unUngroupedUnpolar :: S2 (UngroupedZero r) (UngroupedNonZero r) }
  deriving (Eq, Ord, Show)


newtype UngroupedPolar r p = UngroupedPolar
  { unUngroupedPolar :: Polarity (UngroupedZero r) (UngroupedNonZero r) p }
  deriving (Eq, Ord, Show)

neutralizeUngrouped :: UngroupedPolar r p -> UngroupedUnpolar r
neutralizeUngrouped = undefined

polarizeUngrouped :: p -> UngroupedUnpolar r -> UngroupedPolar r p
polarizeUngrouped = undefined

-- # Grouped

-- No need for a GroupedZero group - there is only one type

newtype GroupedNonZero r = GroupedNonZero
  { unGroupedNonZero :: S5 (MasunoGroupedLeft r)
                           (MasunoGroupedLeftRad r)
                           (MasunoGroupedRight r)
                           (FracunoFirstGroupZ r)
                           (FracunoFirstGroupNZ r) }
  deriving (Eq, Ord, Show)

-- ## Grouped - polar and unpolar

newtype GroupedUnpolar r = GroupedUnpolar
  { unGroupedUnpolar :: S2 (GZ r) (GroupedNonZero r) }
  deriving (Eq, Ord, Show)

newtype GroupedPolar r p = GroupedPolar
  { unGroupedPolar :: Polarity (GZ r) (GroupedNonZero r) p }
  deriving (Eq, Ord, Show)

neutralizeGrouped :: GroupedPolar r p -> GroupedUnpolar r
neutralizeGrouped = undefined

polarizeGrouped :: p -> GroupedUnpolar r -> GroupedPolar r p
polarizeGrouped = undefined

-- # Unpolar

newtype Unpolar r = Unpolar
  { unUnpolar :: S2 (UngroupedUnpolar r) (GroupedUnpolar r) }
  deriving (Eq, Ord, Show)

-- # Polar

newtype Polar r p = Polar
  { unPolar :: S2 (UngroupedPolar r p) (GroupedPolar r p) }
  deriving (Eq, Ord, Show)

neutralize :: Polar r p -> Unpolar r
neutralize = undefined

polarize :: p -> Unpolar r -> Polar r p
polarize = undefined

-- # All abstract types, polar and unpolar

newtype Abstract r p = Abstract
  { unAbstract :: S2 (Unpolar r) (Polar r p) }
  deriving (Eq, Ord, Show)

