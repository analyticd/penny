{-# LANGUAGE RankNTypes #-}
module Penny.Numbers.Abstract.Aggregates.Shrinkers where

import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar.Shrinkers
import Prelude.Shrinkers
import Data.Sums.Shrinkers

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
  :: (forall b. b -> Group r b)
  -> GroupedNonZero r
  -> [GroupedNonZero r]
groupedNonZero g (GroupedNonZero s) = fmap GroupedNonZero $
  s5 (masunoGroupedLeft g) (masunoGroupedLeftRad g)
     (masunoGroupedRight g) (fracunoFirstGroupZ g)
     (fracunoFirstGroupNZ g) s



groupedUnpolar
  :: (forall b. b -> Group r b)
  -> GroupedUnpolar r
  -> [GroupedUnpolar r]
groupedUnpolar g (GroupedUnpolar s) = fmap GroupedUnpolar $
  s2 (gZ g) (groupedNonZero g) s

groupedPolar
  :: (p -> [p])
  -> (forall b. b -> Group r b)
  -> GroupedPolar r p
  -> [GroupedPolar r p]
groupedPolar sp g (GroupedPolar s) = fmap GroupedPolar $
  polarity (gZ g) (groupedNonZero g) sp s

unpolar
  :: (forall b. b -> Group r b)
  -> Unpolar r
  -> [Unpolar r]
unpolar g (Unpolar s) = fmap Unpolar $
  s2 ungroupedUnpolar (groupedUnpolar g) s

polar
  :: (p -> [p])
  -> (forall b. b -> Group r b)
  -> Polar r p
  -> [Polar r p]
polar sp g (Polar s) = fmap Polar $
  s2 (ungroupedPolar sp) (groupedPolar sp g) s

abstract
  :: (p -> [p])
  -> (forall b. b -> Group r b)
  -> Abstract r p
  -> [Abstract r p]
abstract sp g (Abstract a) = fmap Abstract $
  s2 (unpolar g) (polar sp g) a
