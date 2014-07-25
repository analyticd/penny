{-# LANGUAGE RankNTypes #-}
module Penny.Numbers.Abstract.Grouping (group) where

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Sums
import Penny.Numbers.Concrete (NovDecs(..))

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

group
  :: (forall a. a -> Group r a)
  -> UngroupedUnpolar r
  -> Either (UngroupedUnpolar r) (GroupedUnpolar r)
group g o@(UngroupedUnpolar plr) = case plr of
  S2a _ -> Left o
  S2b nz -> groupNonZero g nz

groupNonZero
  :: (forall a. a -> Group r a)
  -> UngroupedNonZero r
  -> Either (UngroupedUnpolar r) (GroupedUnpolar r)
groupNonZero grp o@(UngroupedNonZero s3) = case s3 of
  S3a (UNWhole nds) -> case groupNovDecs grp nds of
    Nothing -> noGroup
    Just gr -> Right (GroupedUnpolar (S2b (GroupedNonZero (S5a gr))))

  S3b (UNWholeRadix nd rdx mdd) -> case groupNovDecs grp nd of
    Nothing -> noGroup
    Just gr -> Right (GroupedUnpolar (S2b (GroupedNonZero (S5b glr))))
      where
        glr = masunoGroupedLeftRad gr rdx mdd

  S3c _ -> noGroup
  where
    noGroup = Left (UngroupedUnpolar (S2b o))

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
  :: (forall a. a -> Group r a)
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
            Just (MasunoGroupedLeft (NovDecs nv nds) (g gr1) (fmap g r2))

  where
    makeGroup (a1, m1) = case m1 of
      Nothing -> DecDecs a1 S.empty
      Just (a2, m2) -> case m2 of
        Nothing -> DecDecs a2 (S.singleton a1)
        Just a3 -> DecDecs a3 (a2 <| a2 <| S.empty)


groupsOf3
  :: ((a, Maybe (a, Maybe a)) -> b)
  -> Seq a
  -> Seq b
groupsOf3 f = go S.empty
  where
    go soFar sq = case S.viewr sq of
      EmptyR -> soFar
      r1 :> a1 -> case S.viewr r1 of
        EmptyR -> f (a1, Nothing) <| soFar
        r2 :> a2 -> case S.viewr r2 of
          EmptyR -> f (a1, Just (a2, Nothing)) <| soFar
          r3 :> a3 ->
            go ((f (a1, Just (a2, Just a3))) <| soFar) r3
