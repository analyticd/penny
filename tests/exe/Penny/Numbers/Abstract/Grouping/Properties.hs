module Penny.Numbers.Abstract.Grouping.Properties where

import Penny.Numbers.Abstract.Grouping
import Penny.Numbers.Abstract.Unpolar
import Deka.Native.Abstract
import qualified Data.Sequence as S
import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Numbers.Abstract.RadGroup.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Shrinkers as S
import Penny.Numbers.Abstract.Aggregates

prop_tenThousandNovDecs :: Property
prop_tenThousandNovDecs = groupNovDecs comma nd === Just mgl
  where
    nd = NovDecs D1 (S.fromList [D0, D0, D0, D0, D0])
    mgl = MasunoGroupedLeft (NovDecs D1 (S.singleton D0))
      (Group comma (DecDecs D0 (S.fromList [D0, D0]))) S.empty

-- | ungrouped -> grouped -> ungrouped, for Period

prop_groupRoundTripPeriod :: Property
prop_groupRoundTripPeriod =
  forAll G.grouperPeriod $ \grp ->
  forAllShrink (G.ungroupedUnpolar (return radPeriod))
    S.ungroupedUnpolar $ \uu ->
  case group grp uu of
    Nothing -> label "could not be grouped" True
    Just gu -> label "was grouped" $
      ungroupGroupedUnpolar gu === uu

-- | ungrouped -> grouped -> ungrouped, for Period

prop_groupRoundTripPeriodNoShrink :: Property
prop_groupRoundTripPeriodNoShrink =
  forAll G.grouperPeriod $ \grp ->
  forAll (G.ungroupedUnpolar (return radPeriod)) $ \uu ->
  case group grp uu of
    Nothing -> label "could not be grouped" True
    Just gu -> label "was grouped" $
      ungroupGroupedUnpolar gu === uu
