module Penny.Numbers.Abstract.Aggregates where

import Data.Sums
import Penny.Numbers.Abstract.Unpolar

-- | Holds polar types, along with their polarity.  The first type
-- variable is the type for neutral types.  The second type variable
-- is for opposing types.  The third type variable is the polarity
-- itself, e.g. Debit or Credit.
data Polar n o p
  = Center n
  | OffCenter p o
  deriving (Eq, Ord, Show)

newtype UngroupedZero r
  = UngroupedZero { unUngroupedZero :: S2 UZBare (UZTrailing r) }
  deriving (Eq, Ord, Show)

newtype UngroupedNonZero r
  = UngroupedNonZero
  { unUngroupedNonZero :: S3 UNWhole (UNWholeRadix r) (UNRadFrac r) }
  deriving (Eq, Ord, Show)

newtype Ungrouped r p = Ungrouped
  { unUngrouped :: Polar (UngroupedZero r) (UngroupedNonZero r) p }
  deriving (Eq, Ord, Show)

-- No need for a GroupedZero group - there is only one type

newtype GroupedNonZero r g = GroupedNonZero
  { unGroupedNonZero :: S5 (MasunoGroupedLeft g)
                           (MasunoGroupedLeftRad r g)
                           (MasunoGroupedRight r g)
                           (FracunoFirstGroupZ r g)
                           (FracunoFirstGroupNZ r g) }
  deriving (Eq, Ord, Show)

newtype Grouped r g p = Grouped
  { unGrouped :: Polar (GZ r g) (GroupedNonZero r g) p }
  deriving (Eq, Ord, Show)
