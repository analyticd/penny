module Penny.Copper.Grouping where

import Control.Monad (join)
import Data.Sequence (Seq, (<|), (|>),
  ViewR(EmptyR, (:>)), viewr)
import qualified Data.Sequence as Seq
import Penny.Copper.Types
import Penny.NonEmpty
import Data.Monoid ((<>))
import qualified Pinchot

-- # Ungrouping

-- ## Nil
ungroupNilRadCom :: NilRadCom t a -> NilUngroupedRadCom t a
ungroupNilRadCom (NilRadCom'NilUngroupedRadCom x) = x
ungroupNilRadCom (NilRadCom'NilGroupedRadCom x)
  = ungroupNilGroupedRadCom x

ungroupNilRadPer :: NilRadPer t a -> NilUngroupedRadPer t a
ungroupNilRadPer (NilRadPer'NilUngroupedRadPer x) = x
ungroupNilRadPer (NilRadPer'NilGroupedRadPer x)
  = ungroupNilGroupedRadPer x

-- ## Brim
ungroupBrimRadCom :: BrimRadCom t a -> BrimUngroupedRadCom t a
ungroupBrimRadCom (BrimRadCom'BrimUngroupedRadCom x) = x
ungroupBrimRadCom (BrimRadCom'BrimGroupedRadCom x)
  = ungroupBrimGroupedRadCom x

ungroupBrimRadPer :: BrimRadPer t a -> BrimUngroupedRadPer t a
ungroupBrimRadPer (BrimRadPer'BrimUngroupedRadPer x) = x
ungroupBrimRadPer (BrimRadPer'BrimGroupedRadPer x)
  = ungroupBrimGroupedRadPer x


-- ## RadCom

-- ### Brim

ungroupBrimGroupedRadCom :: BrimGroupedRadCom t a -> BrimUngroupedRadCom t a
ungroupBrimGroupedRadCom (BGGreaterThanOneRadCom d1 (D0'9'Star ds2) bg1)
  = BUGreaterThanOneRadCom d1 (D0'9'Star leftDigs) mayRadixComDigits
  where
    leftDigs = ds2 <> onLeft
    (onLeft, onRight) = ungroupBG1RadCom bg1
    mayRadixComDigits = RadixComDigits'Opt $ case onRight of
      Left Nothing -> Nothing
      Left (Just rdx) ->
        Just $ RadixComDigits rdx (D0'9'Star Seq.empty)
      Right (rdx, NonEmpty d1 ds) ->
        Just $ RadixComDigits rdx (D0'9'Star (d1 <| ds))

ungroupBrimGroupedRadCom (BGLessThanOneRadCom mayZero rdx bg5)
  = BULessThanOneRadCom mayZero rdx (Zero'Star zs) d1 (D0'9'Star ds)
  where
    (zs, d1, ds) = ungroupBG5RadCom bg5


ungroupBG1RadCom
  :: BG1RadCom t a
  -> (Seq (D0'9 t a),
        Either (Maybe (RadixCom t a)) (RadixCom t a, NonEmpty (D0'9 t a)))
  -- ^ Returns a pair @(a, b)@ where
  --
  -- @a@ is the digits to the left of the radix point
  --
  -- @b@ is the digits to the right of the radix point.  If there
  -- are no digits to the right of the radix point, this is @Left
  -- c@, where @c@ is Just if a radix point appears or Nothing if not.
  -- If there are digits to the right of the radix point, this is a
  -- @Right d@, where @d@ is the radix and a NonEmpty with the digits.

ungroupBG1RadCom (BG1GroupOnLeftRadCom _ d1 (D0'9'Star ds) dss b3)
  = (onLeft, onRight)
  where
    onLeft =  d1 <| (ds <> ungroupDigitGroupsRadCom dss)
    onRight = ungroupBG3RadCom b3

ungroupBG1RadCom (BG1GroupOnRightRadCom rdx d1 (D0'9'Star ds) _
  d2 (D0'9'Star ds2) dss)
  = (onLeft, onRight)
  where
    onLeft = Seq.empty
    onRight = Right (rdx, digs)
      where
        digs = NonEmpty d1 ((ds |> d2) <> ds2 <> ungroupDigitGroupsRadCom dss)

ungroupDigitGroupRadCom :: DigitGroupRadCom t a -> NonEmpty (D0'9 t a)
ungroupDigitGroupRadCom (DigitGroupRadCom _ d1 (D0'9'Star ds))
  = NonEmpty d1 ds

ungroupDigitGroupsRadCom :: DigitGroupRadCom'Star t a -> Seq (D0'9 t a)
ungroupDigitGroupsRadCom (DigitGroupRadCom'Star sq) = case nonEmpty sq of
  Nothing -> Seq.empty
  Just ne -> seqFromNonEmpty . join . fmap ungroupDigitGroupRadCom $ ne

ungroupBG3RadCom
  :: BG3RadCom t a
  -> Either (Maybe (RadixCom t a)) (RadixCom t a, NonEmpty (D0'9 t a))
  -- ^ Returns @Left a@ if there are no digits to the right of the
  -- radix point, with @a@ being Just if there is a radix point,
  -- Nothing if no radix point.  Returns @Right b@ if there are
  -- digits to the right of the radix point, with @b@ being the
  -- radix and a NonEmpty with the digits.

ungroupBG3RadCom BG3NilRadCom = Left Nothing
ungroupBG3RadCom (BG3RadixRadCom rdx b4) = case b4 of
  BG4NilRadCom -> Left (Just rdx)
  BG4DigitRadCom d1 (D0'9'Star ds2) dss -> Right (rdx, digs)
    where
      digs = NonEmpty d1 (ds2 <> ungroupDigitGroupsRadCom dss)


ungroupBG7RadCom :: BG7RadCom t a -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG7RadCom = goBG7 Seq.empty
  where
    goBG7 zeroes (BG7ZeroesRadCom z1 (Zero'Star zs) b8)
      = goBG8 ((zeroes |> z1) <> zs) b8
    goBG7 zeroes (BG7NovemRadCom d1 (D0'9'Star ds) dss)
      = (zeroes, d1, ds <> ungroupDigitGroupsRadCom dss)

    goBG8 zeroes (BG8NovemRadCom d1 (D0'9'Star ds) dss)
      = (zeroes, d1, (ds <> ungroupDigitGroupsRadCom dss))
    goBG8 zeroes (BG8GroupRadCom _ b7) = goBG7 zeroes b7

ungroupBG6RadCom :: BG6RadCom t a -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG6RadCom (BG6NovemRadCom d1 (D0'9'Star ds1) _ d2
  (D0'9'Star ds2) dss)
  = (Seq.empty, d1, (ds1 <> (d2 <| (ds2 <> ungroupDigitGroupsRadCom dss))))

ungroupBG6RadCom (BG6GroupRadCom _ b7) = ungroupBG7RadCom b7

ungroupBG5RadCom :: BG5RadCom t a -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG5RadCom (BG5NovemRadCom d1 (D0'9'Star ds2) _grp3 d4
  (D0'9'Star ds5) ds6)
  = (Seq.empty, d1, (ds2 <> (d4 <| (ds5 <> ungroupDigitGroupsRadCom ds6))))

ungroupBG5RadCom (BG5ZeroRadCom z1 (Zero'Star zs) b6)
  = (zeroes', d1, ds)
  where
    (zeroes, d1, ds) = ungroupBG6RadCom b6
    zeroes' = z1 <| (zs <> zeroes)

-- ### Nil
ungroupNilGroupedRadCom :: NilGroupedRadCom t a -> NilUngroupedRadCom t a
ungroupNilGroupedRadCom (NilGroupedRadCom (Zero'Opt mayZero1) rdx2 z3
  (Zero'Star zs4) (ZeroGroupRadCom'Plus (Pinchot.NonEmpty grp1 grpsRest)))
  = case mayZero1 of
      Nothing -> NURadixRadCom rdx2 z3 (Zero'Star (zs4 <> zeroesFromGroups))
      Just z -> NUZeroRadCom z (RadixZeroesRadCom'Opt (Just
        (RadixZeroesRadCom rdx2 (Zero'Star (z3 <| zs4 <> zeroesFromGroups)))))
      where
        allGroups = NonEmpty grp1 grpsRest
        stripGrouper (ZeroGroupRadCom _ z1 (Zero'Star zs))
          = NonEmpty z1 zs
        zeroesFromGroups = seqFromNonEmpty . join . fmap stripGrouper
          $ allGroups

-- ## RadPer

-- ## Brim

ungroupBrimGroupedRadPer :: BrimGroupedRadPer t a -> BrimUngroupedRadPer t a
ungroupBrimGroupedRadPer (BGGreaterThanOneRadPer d1 (D0'9'Star ds2) bg1)
  = BUGreaterThanOneRadPer d1 (D0'9'Star leftDigs) mayRadixPerDigits
  where
    leftDigs = ds2 <> onLeft
    (onLeft, onRight) = ungroupBG1RadPer bg1
    mayRadixPerDigits = RadixPerDigits'Opt $ case onRight of
      Left Nothing -> Nothing
      Left (Just rdx) ->
        Just $ RadixPerDigits rdx (D0'9'Star Seq.empty)
      Right (rdx, NonEmpty d1 ds) ->
        Just $ RadixPerDigits rdx (D0'9'Star (d1 <| ds))

ungroupBrimGroupedRadPer (BGLessThanOneRadPer mayZero rdx bg5)
  = BULessThanOneRadPer mayZero rdx (Zero'Star zs) d1 (D0'9'Star ds)
  where
    (zs, d1, ds) = ungroupBG5RadPer bg5


ungroupBG1RadPer
  :: BG1RadPer t a
  -> (Seq (D0'9 t a), Either (Maybe (RadixPer t a))
                             (RadixPer t a, NonEmpty (D0'9 t a)))
  -- ^ Returns a pair @(a, b)@ where
  --
  -- @a@ is the digits to the left of the radix point
  --
  -- @b@ is the digits to the right of the radix point.  If there
  -- are no digits to the right of the radix point, this is @Left
  -- c@, where @c@ is Just if a radix point appears or Nothing if not.
  -- If there are digits to the right of the radix point, this is a
  -- @Right d@, where @d@ is the radix and a NonEmpty with the digits.

ungroupBG1RadPer (BG1GroupOnLeftRadPer _ d1 (D0'9'Star ds) dss b3)
  = (onLeft, onRight)
  where
    onLeft =  d1 <| (ds <> ungroupDigitGroupsRadPer dss)
    onRight = ungroupBG3RadPer b3

ungroupBG1RadPer (BG1GroupOnRightRadPer rdx d1 (D0'9'Star ds) _
  d2 (D0'9'Star ds2) dss)
  = (onLeft, onRight)
  where
    onLeft = Seq.empty
    onRight = Right (rdx, digs)
      where
        digs = NonEmpty d1 ((ds |> d2) <> ds2 <> ungroupDigitGroupsRadPer dss)

ungroupDigitGroupRadPer :: DigitGroupRadPer t a -> NonEmpty (D0'9 t a)
ungroupDigitGroupRadPer (DigitGroupRadPer _ d1 (D0'9'Star ds))
  = NonEmpty d1 ds

ungroupDigitGroupsRadPer :: DigitGroupRadPer'Star t a -> Seq (D0'9 t a)
ungroupDigitGroupsRadPer (DigitGroupRadPer'Star sq) = case nonEmpty sq of
  Nothing -> Seq.empty
  Just ne -> seqFromNonEmpty . join . fmap ungroupDigitGroupRadPer $ ne

ungroupBG3RadPer
  :: BG3RadPer t a
  -> Either (Maybe (RadixPer t a)) (RadixPer t a, NonEmpty (D0'9 t a))
  -- ^ Returns @Left a@ if there are no digits to the right of the
  -- radix point, with @a@ being Just if there is a radix point,
  -- Nothing if no radix point.  Returns @Right b@ if there are
  -- digits to the right of the radix point, with @b@ being the
  -- radix and a NonEmpty with the digits.

ungroupBG3RadPer BG3NilRadPer = Left Nothing
ungroupBG3RadPer (BG3RadixRadPer rdx b4) = case b4 of
  BG4NilRadPer -> Left (Just rdx)
  BG4DigitRadPer d1 (D0'9'Star ds2) dss -> Right (rdx, digs)
    where
      digs = NonEmpty d1 (ds2 <> ungroupDigitGroupsRadPer dss)


ungroupBG7RadPer :: BG7RadPer t a -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG7RadPer = goBG7 Seq.empty
  where
    goBG7 zeroes (BG7ZeroesRadPer z1 (Zero'Star zs) b8)
      = goBG8 ((zeroes |> z1) <> zs) b8
    goBG7 zeroes (BG7NovemRadPer d1 (D0'9'Star ds) dss)
      = (zeroes, d1, ds <> ungroupDigitGroupsRadPer dss)

    goBG8 zeroes (BG8NovemRadPer d1 (D0'9'Star ds) dss)
      = (zeroes, d1, (ds <> ungroupDigitGroupsRadPer dss))
    goBG8 zeroes (BG8GroupRadPer _ b7) = goBG7 zeroes b7

ungroupBG6RadPer :: BG6RadPer t a -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG6RadPer (BG6NovemRadPer d1 (D0'9'Star ds1) _ d2
  (D0'9'Star ds2) dss)
  = (Seq.empty, d1, (ds1 <> (d2 <| (ds2 <> ungroupDigitGroupsRadPer dss))))

ungroupBG6RadPer (BG6GroupRadPer _ b7) = ungroupBG7RadPer b7

ungroupBG5RadPer
  :: BG5RadPer t a
  -> (Seq (Zero t a), D1'9 t a, Seq (D0'9 t a))
ungroupBG5RadPer (BG5NovemRadPer d1 (D0'9'Star ds2) _grp3 d4
  (D0'9'Star ds5) ds6)
  = (Seq.empty, d1, (ds2 <> (d4 <| (ds5 <> ungroupDigitGroupsRadPer ds6))))

ungroupBG5RadPer (BG5ZeroRadPer z1 (Zero'Star zs) b6)
  = (zeroes', d1, ds)
  where
    (zeroes, d1, ds) = ungroupBG6RadPer b6
    zeroes' = z1 <| (zs <> zeroes)

-- ### Nil
ungroupNilGroupedRadPer :: NilGroupedRadPer t a -> NilUngroupedRadPer t a
ungroupNilGroupedRadPer (NilGroupedRadPer (Zero'Opt mayZero1) rdx2 z3
  (Zero'Star zs4) (ZeroGroupRadPer'Plus (Pinchot.NonEmpty grp1 grpsRest)))
  = case mayZero1 of
      Nothing -> NURadixRadPer rdx2 z3 (Zero'Star (zs4 <> zeroesFromGroups))
      Just z -> NUZeroRadPer z (RadixZeroesRadPer'Opt (Just
        (RadixZeroesRadPer rdx2 (Zero'Star (z3 <| zs4 <> zeroesFromGroups)))))
      where
        allGroups = NonEmpty grp1 grpsRest
        stripGrouper (ZeroGroupRadPer _ z1 (Zero'Star zs))
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
  :: GrpRadCom t a
  -> BrimUngroupedRadCom t a
  -> Maybe (BrimGroupedRadCom t a)
groupBrimUngroupedRadCom grpr bu = case bu of
  BULessThanOneRadCom {} -> Nothing
  BUGreaterThanOneRadCom d1 (D0'9'Star ds)
    (RadixComDigits'Opt mayDigs) -> case nonEmpty groups of
    Nothing -> Nothing
    Just (NonEmpty (gd1, gd2, gd3) gs) ->
      Just $ BGGreaterThanOneRadCom d1 (D0'9'Star digs) bg1
      where
        digs = case leftovers of
          NoneLeft -> Seq.empty
          OneLeft a -> Seq.singleton a
          TwoLeft a b -> a <| Seq.singleton b
        bg1 = BG1GroupOnLeftRadCom grpr gd1 (D0'9'Star (gd2 <| gd3 <| Seq.empty))
          grps bg3
        grps = DigitGroupRadCom'Star $ fmap makeGroup gs
        makeGroup (dig2, dig1, dig0) =
          DigitGroupRadCom grpr dig2 (D0'9'Star (dig1 <| dig0 <| Seq.empty))
        bg3 = case mayDigs of
          Nothing -> BG3NilRadCom
          Just (RadixComDigits rdx (D0'9'Star digits)) ->
            case nonEmpty digits of
              Nothing -> BG3RadixRadCom rdx BG4NilRadCom
              Just (NonEmpty dig1 digsRest) ->
                BG3RadixRadCom rdx
                  (BG4DigitRadCom dig1 (D0'9'Star digsRest)
                                  (DigitGroupRadCom'Star Seq.empty))
    where
      (leftovers, groups) = groupSeq ds

-- | Groups digits.  Only values greater than 999 are grouped.
-- Digits are grouped into groups of three.  Only digits to the left
-- of the radix point are grouped.  The presence of a radix point,
-- and any digits to the right of the radix point, are not changed.
-- Fails if digits cannot be grouped.  The given grouping character
-- is used.

groupBrimUngroupedRadPer
  :: GrpRadPer t a
  -> BrimUngroupedRadPer t a
  -> Maybe (BrimGroupedRadPer t a)
groupBrimUngroupedRadPer grpr bu = case bu of
  BULessThanOneRadPer {} -> Nothing
  BUGreaterThanOneRadPer d1 (D0'9'Star ds)
    (RadixPerDigits'Opt mayDigs) -> case nonEmpty groups of
    Nothing -> Nothing
    Just (NonEmpty (gd1, gd2, gd3) gs) ->
      Just $ BGGreaterThanOneRadPer d1 (D0'9'Star digs) bg1
      where
        digs = case leftovers of
          NoneLeft -> Seq.empty
          OneLeft a -> Seq.singleton a
          TwoLeft a b -> a <| Seq.singleton b
        bg1 = BG1GroupOnLeftRadPer grpr gd1 (D0'9'Star (gd2 <| gd3 <| Seq.empty))
          grps bg3
        grps = DigitGroupRadPer'Star $ fmap makeGroup gs
        makeGroup (dig2, dig1, dig0) =
          DigitGroupRadPer grpr dig2 (D0'9'Star (dig1 <| dig0 <| Seq.empty))
        bg3 = case mayDigs of
          Nothing -> BG3NilRadPer
          Just (RadixPerDigits rdx (D0'9'Star digits)) ->
            case nonEmpty digits of
              Nothing -> BG3RadixRadPer rdx BG4NilRadPer
              Just (NonEmpty dig1 digsRest) ->
                BG3RadixRadPer rdx
                  (BG4DigitRadPer dig1 (D0'9'Star digsRest)
                                  (DigitGroupRadPer'Star Seq.empty))
    where
      (leftovers, groups) = groupSeq ds

