module Penny.Numbers.Abstract.Grouping where

import Penny.Numbers.Abstract.Unsigned
import qualified Data.Foldable as F
import Penny.Numbers.Natural
import Penny.Numbers.Abstract.RadGroup
import Deka.Native.Abstract
import qualified Data.Sequence as S
import Data.Sequence (Seq, (<|))
import Data.Monoid
import Penny.Numbers.Abstract.Signed

ungroupGrouped
  :: SignedGrouped r p
  -> SignedUngrouped r p
ungroupGrouped (SignedGrouped plrty) = SignedUngrouped $ case plrty of
  Center ng -> Center . ungroupNilGrouped $ ng
  OffCenter oc sd -> OffCenter (ungroupBrimGrouped oc) sd

ungroupSigned
  :: Signed r p
  -> SignedUngrouped r p
ungroupSigned (Signed plrty) = SignedUngrouped $ case plrty of
  Center nil -> Center . ungroupNil $ nil
  OffCenter o p -> OffCenter (ungroupBrim o) p

ungroupedToPolar
  :: SignedUngrouped r p
  -> Signed r p
ungroupedToPolar (SignedUngrouped py) = Signed $ case py of
  Center nu -> Center (NilUngrouped nu)
  OffCenter bu s -> OffCenter (BrimUngrouped bu) s

groupedToPolar
  :: SignedGrouped r p
  -> Signed r p
groupedToPolar (SignedGrouped py) = Signed $ case py of
  Center ng -> Center (NilGrouped ng)
  OffCenter bg s -> OffCenter (BrimGrouped bg) s

ungroupNil
  :: Nil r
  -> NilUngrouped r
ungroupNil n = case n of
  NilUngrouped nu -> nu
  NilGrouped ng -> ungroupNilGrouped ng

ungroupBrim
  :: Brim r
  -> BrimUngrouped r
ungroupBrim b = case b of
  BrimUngrouped bu -> bu
  BrimGrouped bg -> ungroupBrimGrouped bg

-- | Ungroups a 'NilGrouped'.  Retains the leading zero if there is
-- one.
ungroupNilGrouped :: NilGrouped r -> NilUngrouped r
ungroupNilGrouped ng = case ng of
  NGLeadingZero z (NG1 rdx zs1 _ zs2 sq) ->
    NULeadingZero z (NU1Radix rdx (NU2Zeroes zz))
    where
      zz = F.foldl' add (zs1 `addZ` zs2) sq

  NGNoLeadingZero (NG1 rdx zs1 _ zs2 sq) -> NUNoLeadingZero rdx zz
    where
      zz = F.foldl' add (zs1 `addZ` zs2) sq
  where
    add x (_, y) = addZ x y
    addZ (Zeroes x) (Zeroes y) = Zeroes $ addPos x y

-- | Ungroups a 'BrimGrouped'.  Retains the leading zero if there is
-- one.

ungroupBrimGrouped :: BrimGrouped r -> BrimUngrouped r
ungroupBrimGrouped bg = case bg of
  BGMasuno (NE nv ds) bg1 -> case ungroupBG1 bg1 of
    Left (neGroups, mayRdx) -> BUMasuno ne' bu1
      where
        ne' = NE nv (ds <> flatten neGroups)
        bu1 = case mayRdx of
          Nothing -> BU1End
          Just (rdx, mayDecDecs) -> BU1Radix rdx $ case mayDecDecs of
            Nothing -> S.empty
            Just decems -> flatten decems
    Right (rdx, ne) -> BUMasuno (NE nv ds) (BU1Radix rdx (flatten ne))

  BGFracuno bg4 -> BUFracuno bu2
    where
      (mayLeadingZ, rdx, (mayZs, nes)) = ungroupBG4 bg4
      bu2 = case mayLeadingZ of
        Nothing -> BU2NoLeadingZero rdx bu3
        Just z -> BU2LeadingZero z rdx bu3
      bu3 = case mayZs of
        Just zs -> BU3Zeroes zs nes
        Nothing -> BU3NoZeroes nes


ungroupBG1
  :: BG1 r
  -> Either (NE Decem Decem, Maybe (Radix r, Maybe (NE Decem Decem)))
            (Radix r, NE Decem Decem)
ungroupBG1 bg1 = case bg1 of
  BG1GroupOnLeft _ (NE d1 ds1) sq bg2 -> Left (ne', ungroupBG2 bg2)
    where
      ne' = NE d1 . F.foldl' (<>) ds1 . fmap (flatten . snd) $ sq

  BG1GroupOnRight rdx (NE d1 ds1) sq -> Right (rdx, ds')
    where
      ds' = NE d1 . F.foldl' (<>) ds1 . fmap (flatten . snd) $ sq

-- | Ungroups a 'BG2'.  A result of 'Nothing' indicates there is no
-- radix point.  A 'Just' result indicates there is a radix point, and
-- maybe something after it.
ungroupBG2 :: BG2 r -> Maybe (Radix r, (Maybe (NE Decem Decem)))
ungroupBG2 bg2 = case bg2 of
  BG2End -> Nothing
  BG2Radix rdx bg3 -> Just (rdx, ungroupBG3 bg3)

ungroupBG3 :: BG3 r -> Maybe (NE Decem Decem)
ungroupBG3 bg3 = case bg3 of
  BG3End -> Nothing
  BG3AfterRad (NE d1 ds) sq -> Just . NE d1 . F.foldl' (<>) ds
    . fmap (\(_, sqq) -> flatten sqq) $ sq

flattenNovDecsSeq
  :: NE Novem Decem
  -> Seq (r, (NE Decem Decem))
  -> NE Novem Decem
flattenNovDecsSeq (NE nv ds) sq =  NE nv . F.foldl' (<>) ds
  . fmap (\(_, NE d1 dss) -> d1 <| dss) $ sq

ungroupBG4
  :: BG4 r
  -> (Maybe Zero, Radix r, (Maybe Zeroes, NE Novem Decem))
ungroupBG4 (BG4 mz rdx bg5) = (mz, rdx, ungroupBG5 bg5)

ungroupBG5 :: BG5 r -> (Maybe Zeroes, NE Novem Decem)
ungroupBG5 bg5 = case bg5 of
  BG5Novem ne sq -> (Nothing, flattenNovDecsSeq ne (flatten sq))
  BG5Zeroes zs bg6 -> ungroupBG6 (Just zs) bg6

ungroupBG7
  :: Maybe Zeroes
  -- ^ Zeroes seen so far
  -> BG7 r
  -> (Maybe Zeroes, NE Novem Decem)
ungroupBG7 mayzs bg7 = case bg7 of
  BG7Zeroes (Zeroes zs) bg8 -> ungroupBG8 mayzs' bg8
    where
      mayzs' = case mayzs of
        Nothing -> Just (Zeroes zs)
        Just (Zeroes zs') -> Just . Zeroes $ addPos zs zs'
  BG7Novem ne sq -> (mayzs, flattenNovDecsSeq ne sq)

ungroupBG6
  :: Maybe Zeroes
  -- ^ Zeroes seen so far
  -> BG6 r
  -> (Maybe Zeroes, NE Novem Decem)
ungroupBG6 mayzs bg6 = case bg6 of
  BG6Novem ne sq -> (mayzs, flattenNovDecsSeq ne (flatten sq))
  BG6Group _ bg7 -> ungroupBG7 mayzs bg7

ungroupBG8
  :: Maybe Zeroes
  -- ^ Zeroes seen so far
  -> BG8 r
  -> (Maybe Zeroes, NE Novem Decem)
ungroupBG8 mayZ bg8 = case bg8 of
  BG8Novem ne sq -> (mayZ, flattenNovDecsSeq ne sq)
  BG8Group _ bg7 -> ungroupBG7 mayZ bg7

-- Groups digits.  Rules for digit grouping:
--
-- * Digits to the left of the radix are grouped only if there are at
-- least five digits to the left of the radix.  Then, they are grouped
-- into groups of three digits each.
--
-- * Digits to the right of the radix are never grouped.
--
-- As a corollary, zero values are never grouped, as they never have
-- more than one digit to the left of the radix point.

