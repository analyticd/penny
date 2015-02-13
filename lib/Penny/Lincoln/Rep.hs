{-# LANGUAGE FlexibleContexts #-}

-- | Number representations.
--
-- These types contain a context-free grammar for number
-- representations.  A number representation is not suitable for
-- arithmetic; however, it \"remembers\" exactly how the user entered
-- a number, complete with grouping characters and the radix point
-- (which may be a period or a comma.)
module Penny.Lincoln.Rep
  ( -- * Radix and grouping
    Radix(..)
  , Grouper(..)
  , RadCom(..)
  , RadPer(..)

  -- * Nil representations
  -- | These represent numbers whose significand is zero.
  , Nil(..)
  , NilGrouped(..)
  , NilUngrouped(..)

  -- * Brim representations
  --
  -- | These represent numbers whose significand is other than zero.
  , Brim(..)
  , BrimGrouped(..)
  , BrimUngrouped(..)
  , BG1(..)
  , BG5(..)
  , BG6(..)
  , BG7(..)
  , BG8(..)

  -- * Aggregate types
  -- | These types aggregate other types.
  , CenterOrOffCenter(..)
  , changeOffCenterType
  , NilOrBrimPolar(..)
  , NilOrBrimScalar(..)
  , NilOrBrimScalarAnyRadix(..)
  , nilOrBrimScalarAnyRadixToQty
  , RepNonNeutralNoSide(..)

  -- * Qty types
  --
  -- | These types represent quantities (as opposed to prices).
  , QtyRep(..)
  , QtyRepAnyRadix(..)

  -- * Exch types
  --
  -- | These types represent exchanges.
  , ExchRep(..)
  , ExchRepAnyRadix(..)

  -- * Digits
  , module Penny.Lincoln.Rep.Digit

  -- * Grouping
  , groupBrimUngrouped
  , ungroupBrimGrouped
  , ungroupNilGrouped
  ) where

import Control.Monad (join)
import Data.Sequence (Seq, ViewR(..), ViewL(..), (<|), (|>))
import Data.Monoid
import qualified Data.Sequence as S
import Penny.Lincoln.Rep.Digit
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

data Grouper
  = ThinSpace
  | Underscore
  deriving (Eq, Ord, Show)

-- | A radix point of a comma.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a comma.  When used alone, it represents a grouping
-- character, which may be a period or other grouping character.
data RadCom
  = Period
  -- ^ When used as a grouping character, a RadCom can be a period
  | RCGrouper Grouper
  -- ^ When used as a grouping character, a RadCom can also be a
  -- 'ThinSpace' or an 'Underscore'.
  deriving (Eq, Ord, Show)

-- | A radix point of a period.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a period.  When used alone, it represents a grouping
-- character, which may be a comma or other grouping character.
-- | A radix point of a period.
data RadPer
  = Comma
  -- ^ When used as a grouping character, a RadPer can be a comma
  | RPGrouper Grouper
  -- ^ When used as a grouping character, a RadPer can also be a
  -- 'ThinSpace' or an 'Underscore'.

  deriving (Eq, Ord, Show)

-- # Nil

data Nil r
  = NilU (NilUngrouped r)
  | NilG (NilGrouped r)
  deriving (Eq, Ord, Show)

data NilGrouped r
  = NilGrouped (Maybe Zero) (Radix r)
               Zero (Seq Zero) r Zero (Seq Zero)
               (Seq (r, Zero, Seq Zero))
  deriving (Eq, Ord, Show)

data NilUngrouped r
  = NUZero Zero (Maybe (Radix r, Maybe (Zero, Seq Zero)))
  | NURadix (Radix r) Zero (Seq Zero)
  deriving (Eq, Ord, Show)

-- # Brim

data Brim r
  = BrimGrouped (BrimGrouped r)
  | BrimUngrouped (BrimUngrouped r)
  deriving (Eq, Ord, Show)

data BrimUngrouped r
  = BUGreaterThanOne D9 (Seq D9z) (Maybe (Radix r, Seq D9z))
  | BULessThanOne (Maybe Zero) (Radix r) (Seq Zero) D9 (Seq D9z)
  deriving (Eq, Ord, Show)

data BrimGrouped r
  = BGGreaterThanOne D9 (Seq D9z) (BG1 r)
  | BGLessThanOne (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

data BG1 r
  = BG1GroupOnLeft r D9z (Seq D9z) (Seq (r, D9z, Seq D9z))
      (Maybe (Radix r, Maybe (D9z, Seq D9z, Seq (r, D9z, Seq D9z))))
  | BG1GroupOnRight (Radix r) D9z (Seq D9z) r D9z (Seq D9z)
                    (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

data BG5 r
  = BG5Novem D9 (Seq D9z) r D9z (Seq D9z)
                   (Seq (r, D9z, Seq D9z))
  | BG5Zero Zero (Seq Zero) (BG6 r)
  deriving (Eq, Ord, Show)

data BG6 r
  = BG6Novem D9 (Seq D9z) r D9z (Seq D9z)
             (Seq (r, D9z, Seq D9z))
  | BG6Group r (BG7 r)
  deriving (Eq, Ord, Show)

data BG7 r
  = BG7Zeroes Zero (Seq Zero) (BG8 r)
  | BG7Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

data BG8 r
  = BG8Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  | BG8Group r (BG7 r)
  deriving (Eq, Ord, Show)

-- # Others

-- Same as the old Polarity

-- | Objects that can be neutral or, alterntively, can be polar; for
-- example, numbers on a number line that includes zero.
data CenterOrOffCenter n o p
  = Center n
  -- ^ This object is neutral.

  | OffCenter o p
  -- ^ This object is not neutral.  The second field indicates the
  -- polarity of the object, while the first is the object itself.
  deriving (Eq, Ord, Show)

changeOffCenterType
  :: p'
  -> CenterOrOffCenter n o p
  -> Maybe (CenterOrOffCenter n o p')
changeOffCenterType _ (Center _) = Nothing
changeOffCenterType p' (OffCenter o _) = Just $ OffCenter o p'

-- | Number representations that may be neutral or non-neutral.  The
-- type variable is the type of the radix point and grouping
-- character.  Unlike 'NilOrBrimPolar', a 'NilOrBrimScalar' does not
-- have a polarity.

newtype NilOrBrimScalar r
  = NilOrBrimScalar (Either (Nil r) (Brim r))
  deriving (Eq, Ord, Show)

-- | Number types that may be neutral or non-neutral, with either a
-- comma or period radix.  Does not have a polarity.
newtype NilOrBrimScalarAnyRadix
  = NilOrBrimScalarAnyRadix (Either (NilOrBrimScalar RadCom)
                                    (NilOrBrimScalar RadPer))
  deriving (Eq, Ord, Show)

-- | Adds polarity to a 'NilOrBrimScalarAnyRadix' to transform it to a
-- 'QtyRepAnyRadix'.  Nil values do not receive a polarity; all other
-- values are assigned to the given 'Side'.
nilOrBrimScalarAnyRadixToQty
  :: Side
  -- ^ Assign this 'Side' to polar values
  -> NilOrBrimScalarAnyRadix
  -> QtyRepAnyRadix
nilOrBrimScalarAnyRadixToQty s (NilOrBrimScalarAnyRadix e) =
  QtyRepAnyRadix e'
  where
    e' = case e of
      Left (NilOrBrimScalar (Left nilCom)) ->
        Left (QtyRep (NilOrBrimPolar (Center nilCom)))
      Left (NilOrBrimScalar (Right brimCom)) ->
        Left (QtyRep (NilOrBrimPolar (OffCenter brimCom s)))
      Right (NilOrBrimScalar (Left nilPer)) ->
        Right (QtyRep (NilOrBrimPolar (Center nilPer)))
      Right (NilOrBrimScalar (Right brimPer)) ->
        Right (QtyRep (NilOrBrimPolar (OffCenter brimPer s)))

-- Same as old Stokely

-- | Number representations that may be neutral or, alternatively, may
-- be non-neutral.  The first type variable is the type of the radix
-- point and grouping character; see, for example, 'RadCom' or
-- 'RadPer'.  The second type variable is the polarity; see, for
-- example, 'Penny.Lincoln.Side.Side'.

newtype NilOrBrimPolar r p
  = NilOrBrimPolar (CenterOrOffCenter (Nil r) (Brim r) p)
  deriving (Eq, Ord, Show)

-- Same as old Philly

-- | Representations that are non-neutral and have a radix that is
-- either a period or a comma.  Though they are non-neutral, they do
-- not have a side.

newtype RepNonNeutralNoSide
  = RepNonNeutralNoSide
    (Either (Brim RadCom) (Brim RadPer))
    deriving (Eq, Ord, Show)

-- # Qty representations

-- Same as old Walker

-- | Qty representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'Side'.
--
-- This is a complete representation of a quantity; that is, it can
-- represent any quantity.

newtype QtyRep r
  = QtyRep (NilOrBrimPolar r Side)
  deriving (Eq, Ord, Show)

-- Same as old Muddy

-- | Qty representations that may be neutral or non-neutral and have a
-- radix that is either a period or a comma.  If non-neutral, also
-- contains a 'Side'.

newtype QtyRepAnyRadix
  = QtyRepAnyRadix
    (Either (QtyRep RadCom)
            (QtyRep RadPer))
  deriving (Eq, Ord, Show)

-- # Exch representations

-- | Exch representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'PluMin'.
--
-- This is a complete representation of an 'Penny.Lincoln.Exch.Exch';
-- that is, it can represent any 'Penny.Lincoln.Exch.Exch'.

newtype ExchRep r = ExchRep (NilOrBrimPolar r PluMin)
  deriving (Eq, Ord, Show)

-- | Exch representations that may have a radix of 'RadCom' or
-- 'RadPer' and may be neutral or non-neutral.  If non-neutral, also
-- contains a 'PluMin'.

newtype ExchRepAnyRadix = ExchRepAnyRadix
  (Either (ExchRep RadCom) (ExchRep RadPer))
  deriving (Eq, Ord, Show)

-- Grouping

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
        EmptyL -> Nothing
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
