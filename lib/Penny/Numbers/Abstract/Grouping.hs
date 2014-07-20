{-# LANGUAGE RankNTypes #-}
module Penny.Numbers.Abstract.Grouping (group, ungroup) where

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
  :: (forall a. a -> Group g a)
  -> Ungrouped r p
  -> Either (Ungrouped r p) (Grouped r g p)
group g o@(Ungrouped plr) = case plr of
  Center _ -> Left o
  OffCenter p nz -> groupNonZero g p nz

groupNonZero
  :: (forall a. a -> Group g a)
  -> p
  -> UngroupedNonZero r
  -> Either (Ungrouped r p) (Grouped r g p)
groupNonZero grp plr o@(UngroupedNonZero s3) = case s3 of
  S3a (UNWhole nvdcs@(NovDecs _ ds))
    | length ds < 4 -> noGroup
    | otherwise -> Right (Grouped (OffCenter plr gnz))
    where
      gnz = GroupedNonZero (S5a (groupNovDecs grp nvdcs))

  S3b (UNWholeRadix nvdcs@(NovDecs _ ds) rd mayRt)
    | length ds < 4 -> noGroup
    | otherwise -> Right (Grouped (OffCenter plr gnz))
    where
      gnz = GroupedNonZero (S5b glr)
      glr = MasunoGroupedLeftRad (groupNovDecs grp nvdcs) rd suf
      suf = fmap (\x -> (x, [])) mayRt

  S3c _ -> noGroup
  where
    noGroup = Left (Ungrouped (OffCenter plr o))

groupNovDecs
  :: (forall a. a -> Group g a)
  -> NovDecs
  -> MasunoGroupedLeft g
groupNovDecs grp (NovDecs nv ds) = MasunoGroupedLeft g1 g2 gs
  where
    ds1:ds2:dss = headNoMoreThan2 . groupsOf3 $ ds
    g1 = NovDecs nv ds1
    mkGrp ls = grp $ DecDecs (head ls) (tail ls)
    g2 = mkGrp ds2
    gs = map mkGrp dss


ungroup
  :: Grouped r g p
  -> Ungrouped r p
ungroup (Grouped plr) = case plr of
  Center _ -> undefined

-- | Splits a list into groups of 3.  If it doesn't divide evenly,
-- parts at the front will be shorter.

groupsOf3 :: [a] -> [[a]]
groupsOf3 = map reverse . reverse . chunksOf 3 . reverse

-- | Prepends a single item.  If prepending will make the list at the
-- head longer than 3 items, creates a new chunk of items.

prepend :: a -> [[a]] -> [[a]]
prepend a as = case as of
  [] -> [[a]]
  x:xs
    | length x < 3 -> (a : x) : xs
    | otherwise -> [a] : x : xs

-- | Makes sure the first list has no more than 2 items.

headNoMoreThan2 :: [[a]] -> [[a]]
headNoMoreThan2 xs = case xs of
  [] -> []
  a:as
    | length a < 3 -> a:as
    | otherwise -> [head a] : tail a : as
