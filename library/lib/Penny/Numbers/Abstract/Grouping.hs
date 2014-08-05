module Penny.Numbers.Abstract.Grouping where

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Sums


-- | Groups digits.  Rules for digit grouping:
--
-- * Digits to the left of the radix are grouped only if there are at
-- least five digits to the left of the radix.  Then, they are grouped
-- into groups of three digits each.
--
-- * Digits to the right of the radix are never grouped.
--
-- As a corollary, zero values are never grouped, as they never have
-- more than one digit to the left of the radix point.

groupNonZero
  :: Grouper r
  -> UngroupedNonZero r
  -> Maybe (GroupedNonZero r)
groupNonZero grp (UngroupedNonZero s3) = case s3 of
  S3a (UNWhole nds) -> case groupNovDecs grp nds of
    Nothing -> Nothing
    Just gr -> Just (GroupedNonZero (S5a gr))

  S3b (UNWholeRadix nd rdx mdd) -> case groupNovDecs grp nd of
    Nothing -> Nothing
    Just gr -> Just (GroupedNonZero (S5b glr))
      where
        glr = masunoGroupedLeftRad gr rdx mdd

  S3c _ -> Nothing

masunoGroupedLeftRad
  :: MasunoGroupedLeft r
  -> Radix r
  -> Maybe DecDecs
  -> MasunoGroupedLeftRad r
masunoGroupedLeftRad mgl rdx mdd =
  MasunoGroupedLeftRad mgl rdx mayDD
  where
    mayDD = fmap (\dd -> (dd, S.empty)) mdd

groupNovDecs
  :: Grouper r
  -> NovDecs
  -> Maybe (MasunoGroupedLeft r)
groupNovDecs g (NovDecs nv decs) =
  case S.viewl (groupsOf3 makeGroup decs) of
    EmptyL -> Nothing
    (DecDecs d1 ds1) :< r1 -> case S.viewl r1 of
      EmptyL -> Nothing
      gr1 :< r2 -> case S.viewl ds1 of
        EmptyL -> msd (S.singleton d1)
        d2 :< digsRest -> case S.viewl digsRest of
          EmptyL -> msd (d1 <| d2 <| S.empty)
          d3 :< digsRest' -> case S.viewl digsRest' of
            EmptyL -> msd (d1 <| d2 <| d3 <| S.empty)
            _ -> error "groupWhole: too many groups"
        where
          msd nds =
            Just (MasunoGroupedLeft (NovDecs nv nds)
              (Group g gr1) (fmap (Group g) r2))

  where
    makeGroup (a1, m1) = case m1 of
      Nothing -> DecDecs a1 S.empty
      Just (a2, m2) -> case m2 of
        Nothing -> DecDecs a1 (S.singleton a2)
        Just a3 -> DecDecs a1 (a2 <| a3 <| S.empty)


groupsOf3
  :: ((a, Maybe (a, Maybe a)) -> b)
  -- ^ Takes 1, 2, or 3 digits and places them into a group.  The most
  -- significant digit is on the left side.
  -> Seq a
  -> Seq b
groupsOf3 f = go S.empty
  where
    go soFar sq = case S.viewr sq of
      EmptyR -> soFar
      r1 :> a1 -> case S.viewr r1 of
        EmptyR -> f (a1, Nothing) <| soFar
        r2 :> a2 -> case S.viewr r2 of
          EmptyR -> f (a2, Just (a1, Nothing)) <| soFar
          r3 :> a3 ->
            go ((f (a3, Just (a2, Just a1))) <| soFar) r3
