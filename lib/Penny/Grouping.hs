module Penny.Grouping where

import Control.Monad (join)
import Data.Sequence (Seq, (<|), (|>),
  ViewR(EmptyR, (:>)), viewr)
import qualified Data.Sequence as Seq
import Penny.Grammar
import Penny.NonEmpty
import Data.Monoid ((<>))

-- # Ungrouping

-- ## Nil
ungroupNilRadCom :: NilRadCom -> NilUngroupedRadCom
ungroupNilRadCom (NilRadCom'NilUngroupedRadCom x) = x
ungroupNilRadCom (NilRadCom'NilGroupedRadCom x)
  = ungroupNilGroupedRadCom x

ungroupNilRadPer :: NilRadPer -> NilUngroupedRadPer
ungroupNilRadPer (NilRadPer'NilUngroupedRadPer x) = x
ungroupNilRadPer (NilRadPer'NilGroupedRadPer x)
  = ungroupNilGroupedRadPer x

-- ## Brim
ungroupBrimRadCom :: BrimRadCom -> BrimUngroupedRadCom
ungroupBrimRadCom (BrimRadCom'BrimUngroupedRadCom x) = x
ungroupBrimRadCom (BrimRadCom'BrimGroupedRadCom x)
  = ungroupBrimGroupedRadCom x

ungroupBrimRadPer :: BrimRadPer -> BrimUngroupedRadPer
ungroupBrimRadPer (BrimRadPer'BrimUngroupedRadPer x) = x
ungroupBrimRadPer (BrimRadPer'BrimGroupedRadPer x)
  = ungroupBrimGroupedRadPer x


-- ## RadCom

-- ### Brim

ungroupBrimGroupedRadCom :: BrimGroupedRadCom -> BrimUngroupedRadCom
ungroupBrimGroupedRadCom (BGGreaterThanOneRadCom d1 (D0'9'Seq ds2) bg1)
  = BUGreaterThanOneRadCom d1 (D0'9'Seq leftDigs) mayRadixComDigits
  where
    leftDigs = ds2 <> onLeft
    (onLeft, onRight) = ungroupBG1RadCom bg1
    mayRadixComDigits = RadixComDigits'Maybe $ case onRight of
      Left Nothing -> Nothing
      Left (Just rdx) ->
        Just $ RadixComDigits rdx (D0'9'Seq Seq.empty)
      Right (rdx, NonEmpty d1 ds) ->
        Just $ RadixComDigits rdx (D0'9'Seq (d1 <| ds))

ungroupBrimGroupedRadCom (BGLessThanOneRadCom mayZero rdx bg5)
  = BULessThanOneRadCom mayZero rdx (Zero'Seq zs) d1 (D0'9'Seq ds)
  where
    (zs, d1, ds) = ungroupBG5RadCom bg5


ungroupBG1RadCom
  :: BG1RadCom
  -> (Seq D0'9, Either (Maybe RadixCom) (RadixCom, NonEmpty D0'9))
  -- ^ Returns a pair @(a, b)@ where
  --
  -- @a@ is the digits to the left of the radix point
  --
  -- @b@ is the digits to the right of the radix point.  If there
  -- are no digits to the right of the radix point, this is @Left
  -- c@, where @c@ is Just if a radix point appears or Nothing if not.
  -- If there are digits to the right of the radix point, this is a
  -- @Right d@, where @d@ is the radix and a NonEmpty with the digits.

ungroupBG1RadCom (BG1GroupOnLeftRadCom _ d1 (D0'9'Seq ds) dss b3)
  = (onLeft, onRight)
  where
    onLeft =  d1 <| (ds <> ungroupDigitGroupsRadCom dss)
    onRight = ungroupBG3RadCom b3

ungroupBG1RadCom (BG1GroupOnRightRadCom rdx d1 (D0'9'Seq ds) _
  d2 (D0'9'Seq ds2) dss)
  = (onLeft, onRight)
  where
    onLeft = Seq.empty
    onRight = Right (rdx, digs)
      where
        digs = NonEmpty d1 ((ds |> d2) <> ds2 <> ungroupDigitGroupsRadCom dss)

ungroupDigitGroupRadCom :: DigitGroupRadCom -> NonEmpty D0'9
ungroupDigitGroupRadCom (DigitGroupRadCom _ d1 (D0'9'Seq ds))
  = NonEmpty d1 ds

ungroupDigitGroupsRadCom :: DigitGroupRadCom'Seq -> Seq D0'9
ungroupDigitGroupsRadCom (DigitGroupRadCom'Seq sq) = case nonEmpty sq of
  Nothing -> Seq.empty
  Just ne -> seqFromNonEmpty . join . fmap ungroupDigitGroupRadCom $ ne

ungroupBG3RadCom
  :: BG3RadCom
  -> Either (Maybe RadixCom) (RadixCom, NonEmpty D0'9)
  -- ^ Returns @Left a@ if there are no digits to the right of the
  -- radix point, with @a@ being Just if there is a radix point,
  -- Nothing if no radix point.  Returns @Right b@ if there are
  -- digits to the right of the radix point, with @b@ being the
  -- radix and a NonEmpty with the digits.

ungroupBG3RadCom BG3NilRadCom = Left Nothing
ungroupBG3RadCom (BG3RadixRadCom rdx b4) = case b4 of
  BG4NilRadCom -> Left (Just rdx)
  BG4DigitRadCom d1 (D0'9'Seq ds2) dss -> Right (rdx, digs)
    where
      digs = NonEmpty d1 (ds2 <> ungroupDigitGroupsRadCom dss)


ungroupBG7RadCom :: BG7RadCom -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG7RadCom = goBG7 Seq.empty
  where
    goBG7 zeroes (BG7ZeroesRadCom z1 (Zero'Seq zs) b8)
      = goBG8 ((zeroes |> z1) <> zs) b8
    goBG7 zeroes (BG7NovemRadCom d1 (D0'9'Seq ds) dss)
      = (zeroes, d1, ds <> ungroupDigitGroupsRadCom dss)

    goBG8 zeroes (BG8NovemRadCom d1 (D0'9'Seq ds) dss)
      = (zeroes, d1, (ds <> ungroupDigitGroupsRadCom dss))
    goBG8 zeroes (BG8GroupRadCom _ b7) = goBG7 zeroes b7

ungroupBG6RadCom :: BG6RadCom -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG6RadCom (BG6NovemRadCom d1 (D0'9'Seq ds1) _ d2
  (D0'9'Seq ds2) dss)
  = (Seq.empty, d1, (ds1 <> (d2 <| (ds2 <> ungroupDigitGroupsRadCom dss))))

ungroupBG6RadCom (BG6GroupRadCom _ b7) = ungroupBG7RadCom b7

ungroupBG5RadCom :: BG5RadCom -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG5RadCom (BG5NovemRadCom d1 (D0'9'Seq ds2) _grp3 d4
  (D0'9'Seq ds5) ds6)
  = (Seq.empty, d1, (ds2 <> (d4 <| (ds5 <> ungroupDigitGroupsRadCom ds6))))

ungroupBG5RadCom (BG5ZeroRadCom z1 (Zero'Seq zs) b6)
  = (zeroes', d1, ds)
  where
    (zeroes, d1, ds) = ungroupBG6RadCom b6
    zeroes' = z1 <| (zs <> zeroes)

-- ### Nil
ungroupNilGroupedRadCom :: NilGroupedRadCom -> NilUngroupedRadCom
ungroupNilGroupedRadCom (NilGroupedRadCom (Zero'Maybe mayZero1) rdx2 z3
  (Zero'Seq zs4) (ZeroGroupRadCom'Seq1 (grp1, grpsRest)))
  = case mayZero1 of
      Nothing -> NURadixRadCom rdx2 z3 (Zero'Seq (zs4 <> zeroesFromGroups))
      Just z -> NUZeroRadCom z (RadixZeroesRadCom'Maybe (Just
        (RadixZeroesRadCom rdx2 (Zero'Seq (z3 <| zs4 <> zeroesFromGroups)))))
      where
        allGroups = NonEmpty grp1 grpsRest
        stripGrouper (ZeroGroupRadCom _ z1 (Zero'Seq zs))
          = NonEmpty z1 zs
        zeroesFromGroups = seqFromNonEmpty . join . fmap stripGrouper
          $ allGroups

-- ## RadPer

-- ## Brim

ungroupBrimGroupedRadPer :: BrimGroupedRadPer -> BrimUngroupedRadPer
ungroupBrimGroupedRadPer (BGGreaterThanOneRadPer d1 (D0'9'Seq ds2) bg1)
  = BUGreaterThanOneRadPer d1 (D0'9'Seq leftDigs) mayRadixPerDigits
  where
    leftDigs = ds2 <> onLeft
    (onLeft, onRight) = ungroupBG1RadPer bg1
    mayRadixPerDigits = RadixPerDigits'Maybe $ case onRight of
      Left Nothing -> Nothing
      Left (Just rdx) ->
        Just $ RadixPerDigits rdx (D0'9'Seq Seq.empty)
      Right (rdx, NonEmpty d1 ds) ->
        Just $ RadixPerDigits rdx (D0'9'Seq (d1 <| ds))

ungroupBrimGroupedRadPer (BGLessThanOneRadPer mayZero rdx bg5)
  = BULessThanOneRadPer mayZero rdx (Zero'Seq zs) d1 (D0'9'Seq ds)
  where
    (zs, d1, ds) = ungroupBG5RadPer bg5


ungroupBG1RadPer
  :: BG1RadPer
  -> (Seq D0'9, Either (Maybe RadixPer) (RadixPer, NonEmpty D0'9))
  -- ^ Returns a pair @(a, b)@ where
  --
  -- @a@ is the digits to the left of the radix point
  --
  -- @b@ is the digits to the right of the radix point.  If there
  -- are no digits to the right of the radix point, this is @Left
  -- c@, where @c@ is Just if a radix point appears or Nothing if not.
  -- If there are digits to the right of the radix point, this is a
  -- @Right d@, where @d@ is the radix and a NonEmpty with the digits.

ungroupBG1RadPer (BG1GroupOnLeftRadPer _ d1 (D0'9'Seq ds) dss b3)
  = (onLeft, onRight)
  where
    onLeft =  d1 <| (ds <> ungroupDigitGroupsRadPer dss)
    onRight = ungroupBG3RadPer b3

ungroupBG1RadPer (BG1GroupOnRightRadPer rdx d1 (D0'9'Seq ds) _
  d2 (D0'9'Seq ds2) dss)
  = (onLeft, onRight)
  where
    onLeft = Seq.empty
    onRight = Right (rdx, digs)
      where
        digs = NonEmpty d1 ((ds |> d2) <> ds2 <> ungroupDigitGroupsRadPer dss)

ungroupDigitGroupRadPer :: DigitGroupRadPer -> NonEmpty D0'9
ungroupDigitGroupRadPer (DigitGroupRadPer _ d1 (D0'9'Seq ds))
  = NonEmpty d1 ds

ungroupDigitGroupsRadPer :: DigitGroupRadPer'Seq -> Seq D0'9
ungroupDigitGroupsRadPer (DigitGroupRadPer'Seq sq) = case nonEmpty sq of
  Nothing -> Seq.empty
  Just ne -> seqFromNonEmpty . join . fmap ungroupDigitGroupRadPer $ ne

ungroupBG3RadPer
  :: BG3RadPer
  -> Either (Maybe RadixPer) (RadixPer, NonEmpty D0'9)
  -- ^ Returns @Left a@ if there are no digits to the right of the
  -- radix point, with @a@ being Just if there is a radix point,
  -- Nothing if no radix point.  Returns @Right b@ if there are
  -- digits to the right of the radix point, with @b@ being the
  -- radix and a NonEmpty with the digits.

ungroupBG3RadPer BG3NilRadPer = Left Nothing
ungroupBG3RadPer (BG3RadixRadPer rdx b4) = case b4 of
  BG4NilRadPer -> Left (Just rdx)
  BG4DigitRadPer d1 (D0'9'Seq ds2) dss -> Right (rdx, digs)
    where
      digs = NonEmpty d1 (ds2 <> ungroupDigitGroupsRadPer dss)


ungroupBG7RadPer :: BG7RadPer -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG7RadPer = goBG7 Seq.empty
  where
    goBG7 zeroes (BG7ZeroesRadPer z1 (Zero'Seq zs) b8)
      = goBG8 ((zeroes |> z1) <> zs) b8
    goBG7 zeroes (BG7NovemRadPer d1 (D0'9'Seq ds) dss)
      = (zeroes, d1, ds <> ungroupDigitGroupsRadPer dss)

    goBG8 zeroes (BG8NovemRadPer d1 (D0'9'Seq ds) dss)
      = (zeroes, d1, (ds <> ungroupDigitGroupsRadPer dss))
    goBG8 zeroes (BG8GroupRadPer _ b7) = goBG7 zeroes b7

ungroupBG6RadPer :: BG6RadPer -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG6RadPer (BG6NovemRadPer d1 (D0'9'Seq ds1) _ d2
  (D0'9'Seq ds2) dss)
  = (Seq.empty, d1, (ds1 <> (d2 <| (ds2 <> ungroupDigitGroupsRadPer dss))))

ungroupBG6RadPer (BG6GroupRadPer _ b7) = ungroupBG7RadPer b7

ungroupBG5RadPer :: BG5RadPer -> (Seq Zero, D1'9, Seq D0'9)
ungroupBG5RadPer (BG5NovemRadPer d1 (D0'9'Seq ds2) _grp3 d4
  (D0'9'Seq ds5) ds6)
  = (Seq.empty, d1, (ds2 <> (d4 <| (ds5 <> ungroupDigitGroupsRadPer ds6))))

ungroupBG5RadPer (BG5ZeroRadPer z1 (Zero'Seq zs) b6)
  = (zeroes', d1, ds)
  where
    (zeroes, d1, ds) = ungroupBG6RadPer b6
    zeroes' = z1 <| (zs <> zeroes)

-- ### Nil
ungroupNilGroupedRadPer :: NilGroupedRadPer -> NilUngroupedRadPer
ungroupNilGroupedRadPer (NilGroupedRadPer (Zero'Maybe mayZero1) rdx2 z3
  (Zero'Seq zs4) (ZeroGroupRadPer'Seq1 (grp1, grpsRest)))
  = case mayZero1 of
      Nothing -> NURadixRadPer rdx2 z3 (Zero'Seq (zs4 <> zeroesFromGroups))
      Just z -> NUZeroRadPer z (RadixZeroesRadPer'Maybe (Just
        (RadixZeroesRadPer rdx2 (Zero'Seq (z3 <| zs4 <> zeroesFromGroups)))))
      where
        allGroups = NonEmpty grp1 grpsRest
        stripGrouper (ZeroGroupRadPer _ z1 (Zero'Seq zs))
          = NonEmpty z1 zs
        zeroesFromGroups = seqFromNonEmpty . join . fmap stripGrouper
          $ allGroups

-- # Grouping

type Group a = (a, a, a)
data Leftovers a
  = NoneLeft
  | OneLeft a
  | TwoLeft a a 

-- | Splits a Seq into groups of three, from the right.  Returns the
-- groups and any leftovers on the left.
groupSeq
  :: Seq a
  -> (Leftovers a, Seq (Group a))
groupSeq = go Seq.empty
  where
    go acc sq = case viewr sq of
      EmptyR -> (NoneLeft, acc)
      xs1 :> g1 -> case viewr xs1 of
        EmptyR -> (OneLeft g1, acc)
        xs2 :> g2 -> case viewr xs2 of
          EmptyR -> (TwoLeft g2 g1, acc)
          xs3 :> g3 -> go ((g3, g2, g1) <| acc) xs3

-- | Groups digits.  Only values greater than 999 are grouped.
-- Digits are grouped into groups of three.  Only digits to the left
-- of the radix point are grouped.  The presence of a radix point,
-- and any digits to the right of the radix point, are not changed.
-- Fails if digits cannot be grouped.  The given grouping character
-- is used.

groupBrimUngroupedRadCom
  :: GrpRadCom
  -> BrimUngroupedRadCom
  -> Maybe BrimGroupedRadCom
groupBrimUngroupedRadCom grpr bu = case bu of
  BULessThanOneRadCom {} -> Nothing
  BUGreaterThanOneRadCom d1 (D0'9'Seq ds)
    (RadixComDigits'Maybe mayDigs) -> case nonEmpty groups of
    Nothing -> Nothing
    Just (NonEmpty (gd1, gd2, gd3) gs) ->
      Just $ BGGreaterThanOneRadCom d1 (D0'9'Seq digs) bg1
      where
        digs = case leftovers of
          NoneLeft -> Seq.empty
          OneLeft a -> Seq.singleton a
          TwoLeft a b -> a <| Seq.singleton b
        bg1 = BG1GroupOnLeftRadCom grpr gd1 (D0'9'Seq (gd2 <| gd3 <| Seq.empty))
          grps bg3
        grps = DigitGroupRadCom'Seq $ fmap makeGroup gs
        makeGroup (dig2, dig1, dig0) =
          DigitGroupRadCom grpr dig2 (D0'9'Seq (dig1 <| dig0 <| Seq.empty))
        bg3 = case mayDigs of
          Nothing -> BG3NilRadCom
          Just (RadixComDigits rdx (D0'9'Seq digits)) ->
            case nonEmpty digits of
              Nothing -> BG3RadixRadCom rdx BG4NilRadCom
              Just (NonEmpty dig1 digsRest) ->
                BG3RadixRadCom rdx
                  (BG4DigitRadCom dig1 (D0'9'Seq digsRest)
                                  (DigitGroupRadCom'Seq Seq.empty))
    where
      (leftovers, groups) = groupSeq ds

-- | Groups digits.  Only values greater than 999 are grouped.
-- Digits are grouped into groups of three.  Only digits to the left
-- of the radix point are grouped.  The presence of a radix point,
-- and any digits to the right of the radix point, are not changed.
-- Fails if digits cannot be grouped.  The given grouping character
-- is used.

groupBrimUngroupedRadPer
  :: GrpRadPer
  -> BrimUngroupedRadPer
  -> Maybe BrimGroupedRadPer
groupBrimUngroupedRadPer grpr bu = case bu of
  BULessThanOneRadPer {} -> Nothing
  BUGreaterThanOneRadPer d1 (D0'9'Seq ds)
    (RadixPerDigits'Maybe mayDigs) -> case nonEmpty groups of
    Nothing -> Nothing
    Just (NonEmpty (gd1, gd2, gd3) gs) ->
      Just $ BGGreaterThanOneRadPer d1 (D0'9'Seq digs) bg1
      where
        digs = case leftovers of
          NoneLeft -> Seq.empty
          OneLeft a -> Seq.singleton a
          TwoLeft a b -> a <| Seq.singleton b
        bg1 = BG1GroupOnLeftRadPer grpr gd1 (D0'9'Seq (gd2 <| gd3 <| Seq.empty))
          grps bg3
        grps = DigitGroupRadPer'Seq $ fmap makeGroup gs
        makeGroup (dig2, dig1, dig0) =
          DigitGroupRadPer grpr dig2 (D0'9'Seq (dig1 <| dig0 <| Seq.empty))
        bg3 = case mayDigs of
          Nothing -> BG3NilRadPer
          Just (RadixPerDigits rdx (D0'9'Seq digits)) ->
            case nonEmpty digits of
              Nothing -> BG3RadixRadPer rdx BG4NilRadPer
              Just (NonEmpty dig1 digsRest) ->
                BG3RadixRadPer rdx
                  (BG4DigitRadPer dig1 (D0'9'Seq digsRest)
                                  (DigitGroupRadPer'Seq Seq.empty))
    where
      (leftovers, groups) = groupSeq ds

