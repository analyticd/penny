module Penny.Numbers.Abstract.Aggregates.Shrinkers where

import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar.Shrinkers
import Prelude.Shrinkers
import Data.Sums.Shrinkers
import Prelude hiding (either)

polarity
  :: (n -> [n])
  -> (o -> [o])
  -> (p -> [p])
  -> Polarity n o p
  -> [Polarity n o p]
polarity fn fo fp py = case py of
  Center n -> map Center . fn $ n
  OffCenter o p ->
    [OffCenter o' p' | (o', p') <- tuple2 fo fp (o, p) ]


ungroupedZero :: UngroupedZero r -> [UngroupedZero r]
ungroupedZero (UngroupedZero r) = fmap UngroupedZero $
  s2 uZZeroOnly uZTrailing r


ungroupedNonZero :: UngroupedNonZero r -> [UngroupedNonZero r]
ungroupedNonZero (UngroupedNonZero s) = fmap UngroupedNonZero $
  s3 uNWhole uNWholeRadix uNRadFrac s

ungroupedUnpolar :: UngroupedUnpolar r -> [UngroupedUnpolar r]
ungroupedUnpolar (UngroupedUnpolar r) = fmap UngroupedUnpolar $
  s2 ungroupedZero ungroupedNonZero r

ungroupedPolar
  :: (p -> [p])
  -> UngroupedPolar r p
  -> [UngroupedPolar r p]
ungroupedPolar sp (UngroupedPolar p) = fmap UngroupedPolar $
  polarity ungroupedZero ungroupedNonZero sp p

groupedNonZero
  :: GroupedNonZero r
  -> [GroupedNonZero r]
groupedNonZero (GroupedNonZero s) = fmap GroupedNonZero $
  s5 masunoGroupedLeft masunoGroupedLeftRad
     masunoGroupedRight fracunoFirstGroupZ
     fracunoFirstGroupNZ s



groupedUnpolar
  :: GroupedUnpolar r
  -> [GroupedUnpolar r]
groupedUnpolar (GroupedUnpolar s) = fmap GroupedUnpolar $
  s2 gZ groupedNonZero s

groupedPolar
  :: (p -> [p])
  -> GroupedPolar r p
  -> [GroupedPolar r p]
groupedPolar sp (GroupedPolar s) = fmap GroupedPolar $
  polarity gZ groupedNonZero sp s

unpolar
  :: Unpolar r
  -> [Unpolar r]
unpolar (Unpolar s) = fmap Unpolar $
  s2 ungroupedUnpolar groupedUnpolar s

polar
  :: Polar r p
  -> [Polar r p]
polar (Polar s) = fmap Polar $
  s2 (ungroupedPolar (const [])) (groupedPolar (const [])) s

abstract
  :: Abstract r p
  -> [Abstract r p]
abstract (Abstract a) = fmap Abstract $
  s2 unpolar polar a

unpolarEitherRadix
  :: Either (Unpolar a) (Unpolar b)
  -> [Either (Unpolar a) (Unpolar b)]
unpolarEitherRadix = either unpolar unpolar

polarEitherRadix
  :: Either (Polar a s) (Polar b s)
  -> [Either (Polar a s) (Polar b s)]
polarEitherRadix = either polar polar
