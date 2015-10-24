module Penny.Group where

import Control.Monad (join)
import Data.Semigroup ((<>))
import Data.Sequence (Seq, (<|), ViewR(EmptyR, (:>)),
  ViewL(EmptyL, (:<)), (|>))
import qualified Data.Sequence as S

import Penny.Grammar
import Penny.NonEmpty

-- | Things that have grouping characters, allowing all grouping
-- characters to be extracted.
class Grouped a where
  groupers :: a g -> NonEmpty g

-- | Things that might have grouping characters, allowing all grouping
-- characters (if any) to be extracted.
class MayGrouped a where
  mayGroupers :: a g -> Seq g

instance Grouped NilGrouped where
  groupers (NilGrouped _z0 _r1 _z2 _sz3 g4 _z5 _sz6 sq7)
    = NonEmpty g4 (fmap (\(x, _, _) -> x) sq7)

instance MayGrouped Nil where
  mayGroupers (NilG ng) = seqFromNonEmpty $ groupers ng
  mayGroupers _ = S.empty

instance MayGrouped NilUngrouped where
  mayGroupers _ = S.empty

instance MayGrouped Brim where
  mayGroupers (BrimGrouped bg) = seqFromNonEmpty . groupers $ bg
  mayGroupers (BrimUngrouped _) = S.empty

instance MayGrouped BrimUngrouped where
  mayGroupers _ = S.empty

instance Grouped BrimGrouped where
  groupers (BGGreaterThanOne _ _ bg1) = groupers bg1
  groupers (BGLessThanOne _ _ bg5) = groupers bg5

instance Grouped BG1 where
  groupers (BG1GroupOnLeft g1 _ _ sq1 Nothing)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) sq1)
  groupers (BG1GroupOnLeft g1 _ _ sq1 (Just (_, Nothing)))
    = NonEmpty g1 (fmap (\(x, _, _) -> x) sq1)
  groupers (BG1GroupOnLeft g1 _ _ sq1 (Just (_, Just
    (_, _, sq2)))) = NonEmpty g1 (fmap get sq1 <> fmap get sq2)
    where
      get (x, _, _) = x
  groupers (BG1GroupOnRight _ _ _ g1 _ _ sq)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) sq)

instance Grouped BG5 where
  groupers (BG5Novem _ _ g1 _ _ gs)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) gs)
  groupers (BG5Zero _ _ bg6) = groupers bg6

instance Grouped BG6 where
  groupers (BG6Novem _ _ g1 _ _ sq)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) sq)
  groupers (BG6Group g1 bg7) = NonEmpty g1 (mayGroupers bg7)

instance MayGrouped BG7 where
  mayGroupers (BG7Zeroes _ _ bg8) = mayGroupers bg8
  mayGroupers (BG7Novem _ _ sq) = fmap (\(x, _, _) -> x) sq

instance MayGrouped BG8 where
  mayGroupers (BG8Novem _ _ sq) = fmap (\(x, _, _) -> x) sq
  mayGroupers (BG8Group g1 bg7) = g1 <| mayGroupers bg7

groupsOf3 :: Seq a -> (Maybe (a, Maybe a), Seq (a, Seq a))
groupsOf3 = go S.empty
  where
    go acc sq = case S.viewr sq of
      EmptyR -> (Nothing, acc)
      xs1 :> x1 -> case S.viewr xs1 of
        EmptyR -> (Just (x1, Nothing), acc)
        xs2 :> x2 -> case S.viewr xs2 of
          EmptyR -> (Just (x1, Just x2), acc)
          xs3 :> x3 -> go ((x3, S.fromList [x2, x1]) <| acc) xs3

-- | Transforms a BrimUngrouped into a BrimGrouped.  Follows the
-- following rules:
--
-- * digits to the right of the radix point are never grouped
--
-- * digits to the left of the radix point are grouped into groups of
-- 3 digits each
--
-- * no digit grouping is performed for values less than 10000
groupBrimUngrouped
  :: r
  -> BrimUngrouped r
  -> Maybe (BrimGrouped r)
groupBrimUngrouped _ (BULessThanOne _ _ _ _ _) = Nothing
groupBrimUngrouped grpr (BUGreaterThanOne d1 ds mayAfter) =
  let (mayFrontDigs, grps) = groupsOf3 ds in
  case S.viewl grps of
    EmptyL -> Nothing
    (g1fst, g1rst) :< grpRest1 -> case S.viewl grpRest1 of
      EmptyL -> case mayFrontDigs of
        Nothing -> Nothing
        Just (msd, Nothing) -> Just $ BGGreaterThanOne d1
          (S.singleton msd) (BG1GroupOnLeft grpr g1fst g1rst S.empty mayAfter')
        Just (lsd, Just msd) -> Just $ BGGreaterThanOne d1
          (S.fromList [msd, lsd])
          (BG1GroupOnLeft grpr g1fst g1rst S.empty mayAfter')
      g2 :< grpRest2 -> Just $ BGGreaterThanOne d1 firstGroup bg1
        where
          bg1 = BG1GroupOnLeft grpr g1fst g1rst
            (fmap addGrp (g2 <| grpRest2)) mayAfter'
          firstGroup = case mayFrontDigs of
            Nothing -> S.empty
            Just (msd, Nothing) -> S.singleton msd
            Just (lsd, Just msd) -> S.fromList [msd, lsd]

  where
    mayAfter' = case mayAfter of
      Nothing -> Nothing
      Just (r, sq) -> case S.viewl sq of
        EmptyL -> Just (r, Nothing)
        x :< xs -> Just (r, Just (x, xs, S.empty))
    addGrp (a, b) = (grpr, a, b)

-- Ungrouping

ungroupBrimGrouped :: BrimGrouped r -> BrimUngrouped r

ungroupBrimGrouped (BGGreaterThanOne d1 s2
  (BG1GroupOnLeft _g3 d4 s5 st6 Nothing))
  = BUGreaterThanOne d1
    ((s2 |> d4) <> s5 <>
      join (fmap (\(_g7, d8, ds9) -> d8 <| ds9) st6)) Nothing

ungroupBrimGrouped (BGGreaterThanOne d1 s2
  (BG1GroupOnLeft _g3 d4 s5 st6 (Just (rdx7, Nothing))))
  = BUGreaterThanOne d1
    ((s2 |> d4) <> s5 <>
      join (fmap (\(_g7, d8, ds9) -> d8 <| ds9) st6)) (Just  (rdx7, S.empty))

ungroupBrimGrouped (BGGreaterThanOne d1 s2
  (BG1GroupOnLeft _g3 d4 s5 st6 (Just (rdx7, Just (d8, s9, st10)))))
  = BUGreaterThanOne d1
    ((s2 |> d4) <> s5 <>
      join (fmap (\(_g11, d12, ds13) -> d12 <| ds13) st6))
      (Just (rdx7, (d8 <| s9) <>
        (join $ fmap (\(_g14, d15, ds16) -> d15 <| ds16) st10)))

ungroupBrimGrouped (BGGreaterThanOne d1 s2
  (BG1GroupOnRight rd3 d4 s5 _g6 d7 ds8 s9))
  = BUGreaterThanOne d1 s2 (Just (rd3,
    (((d4 <| s5) |> d7) <> ds8 <>
      (join (fmap (\(_s10, d11, ds12) -> d11 <| ds12) s9)))))

ungroupBrimGrouped (BGLessThanOne may1 rdx2 (BG5Novem d3 ds4 _g5
  d6 ds7 sq8))
  = BULessThanOne may1 rdx2 S.empty d3 ((ds4 |> d6) <> ds7
    <> join (fmap (\(_g9, s10, ds11) -> s10 <| ds11) sq8))

ungroupBrimGrouped (BGLessThanOne may1 rdx2 (BG5Zero z3 zs4
  (BG6Novem d5 ds6 _g7 d8 ds9 sq10)))
  = BULessThanOne may1 rdx2 (z3 <| zs4) d5 (ds6 <> (d8 <| ds9)
    <> join (fmap (\(_g11, d12, ds13) -> d12 <| ds13) sq10))

ungroupBrimGrouped (BGLessThanOne may1 rdx2 (BG5Zero z3 zs4
  (BG6Group _g5 bg7'6)))
  = BULessThanOne may1 rdx2
    ((z3 <| zs4) <> zs5) d6 ds7
  where
    (zs5, d6, ds7) = flattenBG7 bg7'6

flattenBG7 :: BG7 r -> (Seq Zero, D9, Seq D9z)
flattenBG7 = go S.empty
  where
    go acc (BG7Zeroes z zs bg8) = goBG8 ((acc |> z) <> zs) bg8
    go acc (BG7Novem d1 ds sq) = (acc, d1, ds <>
      join (fmap (\(_, d1', ds') -> d1' <| ds') sq))
    goBG8 acc (BG8Novem d1 ds dss) =
      (acc, d1, ds <> (join (fmap (\(_, d', ds') -> d' <| ds') dss)))
    goBG8 acc (BG8Group _ bg7) = go acc bg7

ungroupNilGrouped :: NilGrouped r -> NilUngrouped r
ungroupNilGrouped (NilGrouped may1 rdx2 z3 zs4 _g5 z6 zs7 sq8)
  = case may1 of
    Just z1 -> NUZero z1 (Just (rdx2, Just (z3,
      (zs4 <> (z6 <| zs7))
      <> join (fmap (\(_g9, z10, z11) -> z10 <| z11) sq8))))
    Nothing -> NURadix rdx2 z3 (zs4 <> (z6 <| zs7)
      <> join (fmap (\(_g9, z10, z11) -> z10 <| z11) sq8))
