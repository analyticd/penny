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

  -- * Grouped - low-level aggregates

  -- | There is no need for a GroupedZero group, as there is only one
  -- type of grouped zero.
  , GroupedNonZero(..)

  -- * Grouped - polar and unpolar
  , GroupedUnpolar(..)
  , GroupedPolar(..)
  , neutralizeGrouped

  -- * All unpolar and polar
  , Unpolar(..)
  , Polar(..)
  , neutralize

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

-- # Grouped

-- No need for a GroupedZero group - there is only one type

newtype GroupedNonZero r g = GroupedNonZero
  { unGroupedNonZero :: S5 (MasunoGroupedLeft g)
                           (MasunoGroupedLeftRad r g)
                           (MasunoGroupedRight r g)
                           (FracunoFirstGroupZ r g)
                           (FracunoFirstGroupNZ r g) }
  deriving (Eq, Ord, Show)

-- ## Grouped - polar and unpolar

newtype GroupedUnpolar r g = GroupedUnpolar
  { unGroupedUnpolar :: S2 (GZ r g) (GroupedNonZero r g) }
  deriving (Eq, Ord, Show)

newtype GroupedPolar r g p = GroupedPolar
  { unGroupedPolar :: Polarity (GZ r g) (GroupedNonZero r g) p }
  deriving (Eq, Ord, Show)

neutralizeGrouped :: GroupedPolar r g p -> GroupedUnpolar r g
neutralizeGrouped = undefined

-- # Unpolar

newtype Unpolar r g = Unpolar
  { unUnpolar :: S2 (UngroupedUnpolar r) (GroupedUnpolar r g) }
  deriving (Eq, Ord, Show)

-- # Polar

newtype Polar r g p = Polar
  { unPolar :: S2 (UngroupedPolar r p) (GroupedPolar r g p) }
  deriving (Eq, Ord, Show)

neutralize :: Polar r g p -> Unpolar r g
neutralize = undefined

-- # All abstract types, polar and unpolar

newtype Abstract r g p = Abstract
  { unAbstract :: S2 (Unpolar r g) (Polar r g p) }
  deriving (Eq, Ord, Show)
