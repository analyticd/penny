module Penny.Numbers.Abstract.Grouping.Properties where

import Data.Maybe
import Data.Sums
import Penny.Numbers.Abstract.Grouping
import Penny.Numbers.Abstract.Unpolar
import Deka.Native.Abstract
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Numbers.Abstract.RadGroup.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Generators as G
import qualified Penny.Numbers.Abstract.Aggregates.Shrinkers as S
import Penny.Numbers.Abstract.Aggregates

prop_tenThousandNovDecs :: Property
prop_tenThousandNovDecs = once $ groupNovDecs comma nd === Just mgl
  where
    nd = NovDecs D1 (S.fromList [D0, D0, D0, D0])
    mgl = MasunoGroupedLeft (NovDecs D1 (S.singleton D0))
      (Group comma (DecDecs D0 (S.fromList [D0, D0]))) S.empty

-- | ungrouped -> grouped -> ungrouped

prop_groupNonZeroRoundTrip :: Property
prop_groupNonZeroRoundTrip =
  forAll G.grouperComma $ \grp ->
  forAllShrink (G.ungroupedNonZero (return radComma))
    S.ungroupedNonZero $ \unz ->
  let r = groupNonZero grp unz in
  isJust r ==> ungroupGroupedNonZero (fromJust r) === unz

-- | grouping succeeds and fails when it should

prop_groupingSuccessOrFailure :: Property
prop_groupingSuccessOrFailure =
  forAll G.grouperComma $ \grp ->
  forAllShrink (G.ungroupedNonZero (return radComma))
    S.ungroupedNonZero $ \uu ->
  let r = groupNonZero grp uu in
  case unUngroupedNonZero uu of
    S3a (UNWhole (NovDecs _ ds))
      | S.length ds > 3 -> isJust r
      | otherwise -> isNothing r
    S3b (UNWholeRadix (NovDecs _ ds) _ _)
      | S.length ds > 3 -> isJust r
      | otherwise -> isNothing r
    S3c _ -> isNothing r

-- | groupsOf3 produces groups that, when ungrouped, give original
-- sequence

prop_reverseGroupsOf3 :: [Int] -> Property
prop_reverseGroupsOf3 ls = flatten rslt === ls
  where
    rslt = groupsOf3 f (S.fromList ls)
    f (a1, maya2) = case maya2 of
      Nothing -> [a1]
      Just (a2, maya3) -> case maya3 of
        Nothing -> [a1, a2]
        Just a3 -> [a1, a2, a3]
    flatten = concat . F.toList
