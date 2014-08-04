module Penny.Numbers.Abstract.Grouping.Properties where

import Penny.Numbers.Abstract.Grouping
import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Numbers.Abstract.RadGroup.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Shrinkers as S
import Penny.Numbers.Abstract.Aggregates

-- | ungrouped -> grouped -> ungrouped, for Period

prop_groupRoundTripPeriod :: Property
prop_groupRoundTripPeriod =
  forAll G.grouperPeriod $ \grp ->
  forAllShrink (G.ungroupedUnpolar (return radPeriod))
    S.ungroupedUnpolar $ \uu ->
  case group grp uu of
    Nothing -> label "could not be grouped" True
    Just gu -> label "was grouped" $
      ungroupGroupedUnpolar gu == uu
