module Penny.Numbers.Abstract.Aggregates.Coarbitrary where

import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar.Coarbitrary
import Test.QuickCheck
import Barecheck.Util
import Data.Sums.Coarbitrary

polarity
  :: (n -> Gen r -> Gen r)
  -> (o -> Gen r -> Gen r)
  -> (p -> Gen r -> Gen r)
  -> Polarity n o p
  -> Gen r
  -> Gen r
polarity fn fo fp py = case py of
  Center n -> varInt 0 . fn n
  OffCenter o p -> varInt 1 . fo o . fp p

ungroupedZero :: UngroupedZero r -> Gen b -> Gen b
ungroupedZero (UngroupedZero s)
  = s2 uZZeroOnly uZTrailing s

ungroupedNonZero :: UngroupedNonZero r -> Gen b -> Gen b
ungroupedNonZero (UngroupedNonZero s) =
  s3 uNWhole uNWholeRadix uNRadFrac s

ungroupedUnpolar :: UngroupedUnpolar r -> Gen b -> Gen b
ungroupedUnpolar (UngroupedUnpolar s) =
  s2 ungroupedZero ungroupedNonZero s

ungroupedPolar
  :: (p -> Gen b -> Gen b)
  -> UngroupedPolar r p
  -> Gen b
  -> Gen b
ungroupedPolar fp (UngroupedPolar py) =
  polarity ungroupedZero ungroupedNonZero fp py

groupedNonZero :: GroupedNonZero r -> Gen b -> Gen b
groupedNonZero (GroupedNonZero s) =
  s5 masunoGroupedLeft masunoGroupedLeftRad masunoGroupedRight
     fracunoFirstGroupZ fracunoFirstGroupNZ s

groupedUnpolar :: GroupedUnpolar r -> Gen b -> Gen b
groupedUnpolar (GroupedUnpolar s) =
  s2 gZ groupedNonZero s

groupedPolar
  :: (p -> Gen b -> Gen b)
  -> GroupedPolar r p
  -> Gen b
  -> Gen b
groupedPolar fp (GroupedPolar py)
  = polarity gZ groupedNonZero fp py

unpolar :: Unpolar r -> Gen b -> Gen b
unpolar (Unpolar s) =
  s2 ungroupedUnpolar groupedUnpolar s

polar :: (p -> Gen b -> Gen b) -> Polar r p -> Gen b -> Gen b
polar fp (Polar s) = s2 (ungroupedPolar fp) (groupedPolar fp) s

abstract
  :: (p -> Gen b -> Gen b) -> Abstract r p -> Gen b -> Gen b
abstract fp (Abstract s) = s2 unpolar (polar fp) s
