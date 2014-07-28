module Penny.Numbers.Abstract.Aggregates
  ( -- * Polarity
    Polarity(..)
  , casePolarity
  , mapPolarity

    -- * Ungrouped - low-level aggregates
  , UngroupedZero(..)
  , UngroupedNonZero(..)

  -- * Ungrouped - polar and unpolar
  , UngroupedUnpolar(..)
  , UngroupedPolar(..)
  , neutralizeUngroupedPolar
  , polarizeUngroupedUnpolar

  -- * Grouped - low-level aggregates

  -- | There is no need for a GroupedZero group, as there is only one
  -- type of grouped zero.
  , GroupedNonZero(..)

  -- * Grouped - polar and unpolar
  , GroupedUnpolar(..)
  , GroupedPolar(..)
  , neutralizeGroupedPolar
  , polarizeGroupedUnpolar
  , ungroupGroupedPolar
  , ungroupGroupedUnpolar

  -- * All unpolar and polar
  , Unpolar(..)
  , Polar(..)
  , neutralizePolar
  , polarizeUnpolar
  , ungroupPolar
  , ungroupUnpolar

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
  | OffCenter o p
  deriving (Eq, Ord, Show)

casePolarity :: (n -> r) -> (o -> p -> r) -> Polarity n o p -> r
casePolarity fn fo a = case a of
  Center n -> fn n
  OffCenter p o -> fo p o

mapPolarity
  :: (n -> n')
  -> (o -> o')
  -> Polarity n o p
  -> Polarity n' o' p
mapPolarity fn fo py = case py of
  Center n -> Center (fn n)
  OffCenter o p -> OffCenter (fo o) p

-- # Ungrouped

newtype UngroupedZero r
  = UngroupedZero { unUngroupedZero :: S2 UZZeroOnly (UZTrailing r) }
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

neutralizeUngroupedPolar :: UngroupedPolar r p -> UngroupedUnpolar r
neutralizeUngroupedPolar (UngroupedPolar py) = UngroupedUnpolar $ case py of
  Center uz -> S2a uz
  OffCenter unz _ -> S2b unz

polarizeUngroupedUnpolar :: p -> UngroupedUnpolar r -> UngroupedPolar r p
polarizeUngroupedUnpolar p (UngroupedUnpolar s) =
  UngroupedPolar $ case s of
    S2a uz -> Center uz
    S2b unz -> OffCenter unz p

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


neutralizeGroupedPolar :: GroupedPolar r p -> GroupedUnpolar r
neutralizeGroupedPolar (GroupedPolar py) = GroupedUnpolar $ case py of
  Center gz -> S2a gz
  OffCenter o _ -> S2b o

polarizeGroupedUnpolar :: p -> GroupedUnpolar r -> GroupedPolar r p
polarizeGroupedUnpolar p (GroupedUnpolar s) = GroupedPolar $ case s of
  S2a gz -> Center gz
  S2b o -> OffCenter o p

ungroupGroupedPolar :: GroupedPolar r p -> UngroupedPolar r p
ungroupGroupedPolar = undefined

ungroupGroupedUnpolar :: GroupedUnpolar r -> UngroupedUnpolar r
ungroupGroupedUnpolar = undefined

-- # Unpolar

newtype Unpolar r = Unpolar
  { unUnpolar :: S2 (UngroupedUnpolar r) (GroupedUnpolar r) }
  deriving (Eq, Ord, Show)

-- # Polar

newtype Polar r p = Polar
  { unPolar :: S2 (UngroupedPolar r p) (GroupedPolar r p) }
  deriving (Eq, Ord, Show)

neutralizePolar :: Polar r p -> Unpolar r
neutralizePolar = undefined

polarizeUnpolar :: p -> Unpolar r -> Polar r p
polarizeUnpolar = undefined

ungroupPolar :: Polar r p -> UngroupedPolar r p
ungroupPolar = undefined

ungroupUnpolar :: Unpolar r -> UngroupedUnpolar r
ungroupUnpolar = undefined

-- # All abstract types, polar and unpolar

newtype Abstract r p = Abstract
  { unAbstract :: S2 (Unpolar r) (Polar r p) }
  deriving (Eq, Ord, Show)

