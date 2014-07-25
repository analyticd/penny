{-# LANGUAGE RankNTypes #-}
module Penny.Numbers.Abstract.Grouping (group, ungroup) where

import qualified Data.Sequence as S
import Deka.Native
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Sums
import Data.List.Split (chunksOf)

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
  S3a (UNWhole nvdcs@(NovDecs _ ds))
    | S.length ds < 4 -> noGroup
    | otherwise -> Right (GroupedUnpolar (S2b gnz))
    where
      gnz = GroupedNonZero (S5a (groupNovDecs grp nvdcs))

  S3b (UNWholeRadix nvdcs@(NovDecs _ ds) rd mayRt)
    | S.length ds < 4 -> noGroup
    | otherwise -> Right (GroupedUnpolar (S2b gnz))
    where
      gnz = GroupedNonZero (S5b glr)
      glr = MasunoGroupedLeftRad (groupNovDecs grp nvdcs) rd suf
      suf = fmap (\x -> (x, S.empty)) mayRt

  S3c _ -> noGroup
  where
    noGroup = Left (UngroupedUnpolar (S2b o))

groupNovDecs
  :: (forall a. a -> Group g a)
  -> NovDecs
  -> MasunoGroupedLeft g
groupNovDecs = undefined

ungroup
  :: GroupedUnpolar r
  -> UngroupedUnpolar r
ungroup = undefined

-- | Splits a list into groups of 3.  If it doesn't divide evenly,
-- parts at the front will be shorter.

groupsOf3 :: [a] -> [[a]]
groupsOf3 = map reverse . reverse . chunksOf 3 . reverse

-- | Makes sure the first list has no more than 2 items.

headNoMoreThan2 :: [[a]] -> [[a]]
headNoMoreThan2 xs = case xs of
  [] -> []
  a:as
    | length a < 3 -> a:as
    | otherwise -> [head a] : tail a : as
