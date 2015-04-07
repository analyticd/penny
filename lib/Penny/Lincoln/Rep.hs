{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- | Number representations.
--
-- These types contain a context-free grammar for number
-- representations.  A number representation is not suitable for
-- arithmetic; however, it \"remembers\" exactly how the user entered
-- a number, complete with grouping characters and the radix point
-- (which may be a period or a comma.)
--
-- To create representations from decimal types, consult the functions
-- in "Penny.Lincoln.Decimal" and "Penny.Lincoln.Qty".  Functions in
-- this module will group some representation types--that is, insert
-- grouping characters to make them easier to read.
module Penny.Lincoln.Rep
  ( -- * Radix and grouping
    Radix(..)
  , Grouper(..)
  , RadCom(..)
  , RadPer(..)
  , Grouped(..)
  , MayGrouped(..)

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

  -- ** Conversions between aggregate types
  , c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  , c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide

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
  , module Penny.Number.Digit

  -- * Grouping
  , groupBrimUngrouped
  , ungroupBrimGrouped
  , ungroupNilGrouped
  ) where

import Control.Monad (join)
import Data.Sequence (Seq, ViewR(..), ViewL(..), (<|), (|>))
import Data.Monoid
import qualified Data.Sequence as S
import Penny.Number.Digit
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin
import Penny.Lincoln.NonEmpty
import Penny.Display

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

data Grouper
  = ThinSpace
  | Underscore
  deriving (Eq, Ord, Show)

instance Display Grouper where
  display ThinSpace = ('\x2009':)
  display Underscore = ('_':)

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

instance Display RadCom where
  display Period = ('.':)
  display (RCGrouper g) = display g

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

instance Display RadPer where
  display Comma = (',':)
  display (RPGrouper g) = display g

-- | Things that have grouping characters, allowing all grouping
-- characters to be extracted.
class Grouped a where
  groupers :: a g -> NonEmpty g

-- | Things that might have grouping characters, allowing all grouping
-- characters (if any) to be extracted.
class MayGrouped a where
  mayGroupers :: a g -> Seq g

-- # Nil

data Nil r
  = NilU (NilUngrouped r)
  | NilG (NilGrouped r)
  deriving (Eq, Ord, Show)

instance MayGrouped Nil where
  mayGroupers (NilG ng) = seqFromNonEmpty $ groupers ng
  mayGroupers _ = S.empty

instance Display (Nil RadCom) where
  display (NilU x) = display x
  display (NilG x) = display x

instance Display (Nil RadPer) where
  display (NilU x) = display x
  display (NilG x) = display x

data NilGrouped r
  = NilGrouped (Maybe Zero) (Radix r)
               Zero (Seq Zero) r Zero (Seq Zero)
               (Seq (r, Zero, Seq Zero))
  deriving (Eq, Ord, Show)

instance Grouped NilGrouped where
  groupers (NilGrouped _z0 _r1 _z2 _sz3 g4 _z5 _sz6 sq7)
    = NonEmpty g4 (fmap (\(x, _, _) -> x) sq7)

instance Display (NilGrouped RadCom) where
  display (NilGrouped may1 _rdx2 z3 zs4 g5 z6 zs7 sq8)
    = display may1 . (',':) . display z3 . display zs4 . display g5
      . display z6 . display zs7 . display sq8

instance Display (NilGrouped RadPer) where
  display (NilGrouped may1 _rdx2 z3 zs4 g5 z6 zs7 sq8)
    = display may1 . ('.':) . display z3 . display zs4 . display g5
      . display z6 . display zs7 . display sq8

data NilUngrouped r
  = NUZero Zero (Maybe (Radix r, Maybe (Zero, Seq Zero)))
  | NURadix (Radix r) Zero (Seq Zero)
  deriving (Eq, Ord, Show)

instance MayGrouped NilUngrouped where
  mayGroupers _ = S.empty

instance Display (NilUngrouped RadCom) where
  display (NUZero z may) = display z . case may of
    Nothing -> id
    Just (_rdx, may2) -> (',':) . display may2
  display (NURadix _rdx1 z2 zs3) = (',':) . display z2 . display zs3

instance Display (NilUngrouped RadPer) where
  display (NUZero z may) = display z . case may of
    Nothing -> id
    Just (_rdx, may2) -> ('.':) . display may2
  display (NURadix _rdx1 z2 zs3) = ('.':) . display z2 . display zs3

-- # Brim

data Brim r
  = BrimGrouped (BrimGrouped r)
  | BrimUngrouped (BrimUngrouped r)
  deriving (Eq, Ord, Show)

instance MayGrouped Brim where
  mayGroupers (BrimGrouped bg) = seqFromNonEmpty . groupers $ bg
  mayGroupers (BrimUngrouped _) = S.empty

instance Display (Brim RadCom) where
  display (BrimGrouped bg) = display bg
  display (BrimUngrouped bu) = display bu

instance Display (Brim RadPer) where
  display (BrimGrouped bg) = display bg
  display (BrimUngrouped bu) = display bu

data BrimUngrouped r
  = BUGreaterThanOne D9 (Seq D9z) (Maybe (Radix r, Seq D9z))
  | BULessThanOne (Maybe Zero) (Radix r) (Seq Zero) D9 (Seq D9z)
  deriving (Eq, Ord, Show)

instance MayGrouped BrimUngrouped where
  mayGroupers _ = S.empty

instance Display (BrimUngrouped RadCom) where
  display (BUGreaterThanOne d1 sq2 may3) = display d1 . display sq2 .
    case may3 of
      Nothing -> id
      Just (_rdx, sq4) -> (',':) . display sq4
  display (BULessThanOne may1 _rdx2 sq3 d4 ds5) =
    display may1 . (',':) . display sq3 . display d4 . display ds5

instance Display (BrimUngrouped RadPer) where
  display (BUGreaterThanOne d1 sq2 may3) = display d1 . display sq2 .
    case may3 of
      Nothing -> id
      Just (_rdx, sq4) -> ('.':) . display sq4
  display (BULessThanOne may1 _rdx2 sq3 d4 ds5) =
    display may1 . ('.':) . display sq3 . display d4 . display ds5

data BrimGrouped r
  = BGGreaterThanOne D9 (Seq D9z) (BG1 r)
  | BGLessThanOne (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

instance Grouped BrimGrouped where
  groupers (BGGreaterThanOne _ _ bg1) = groupers bg1
  groupers (BGLessThanOne _ _ bg5) = groupers bg5

instance Display (BrimGrouped RadCom) where
  display (BGGreaterThanOne d1 sq2 bg1'3) =
    display d1 . display sq2 . display bg1'3
  display (BGLessThanOne m1 _rdx2 bg5'3) =
    display m1 . (',':) . display bg5'3

instance Display (BrimGrouped RadPer) where
  display (BGGreaterThanOne d1 sq2 bg1'3) =
    display d1 . display sq2 . display bg1'3
  display (BGLessThanOne m1 _rdx2 bg5'3) =
    display m1 . ('.':) . display bg5'3

data BG1 r
  = BG1GroupOnLeft r D9z (Seq D9z) (Seq (r, D9z, Seq D9z))
      (Maybe (Radix r, Maybe (D9z, Seq D9z, Seq (r, D9z, Seq D9z))))
  | BG1GroupOnRight (Radix r) D9z (Seq D9z) r D9z (Seq D9z)
                    (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

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

instance Display (BG1 RadCom) where
  display (BG1GroupOnLeft g1 d2 sq3 sq4 may5) =
    display g1 . display d2 . display sq3 . display sq4 . case may5 of
      Nothing -> id
      Just (_rdx, may6) -> (',':) . display may6

  display (BG1GroupOnRight _rdx1 d2 sq3 g4 d5 sq6 sq7) =
    (',':) . display d2 . display sq3 . display g4 . display d5
    . display sq6 . display sq7

instance Display (BG1 RadPer) where
  display (BG1GroupOnLeft g1 d2 sq3 sq4 may5) =
    display g1 . display d2 . display sq3 . display sq4 . case may5 of
      Nothing -> id
      Just (_rdx, may6) -> ('.':) . display may6

  display (BG1GroupOnRight _rdx1 d2 sq3 g4 d5 sq6 sq7) =
    ('.':) . display d2 . display sq3 . display g4 . display d5
    . display sq6 . display sq7


data BG5 r
  = BG5Novem D9 (Seq D9z) r D9z (Seq D9z)
                   (Seq (r, D9z, Seq D9z))
  | BG5Zero Zero (Seq Zero) (BG6 r)
  deriving (Eq, Ord, Show)

instance Grouped BG5 where
  groupers (BG5Novem _ _ g1 _ _ gs)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) gs)
  groupers (BG5Zero _ _ bg6) = groupers bg6

instance Display r => Display (BG5 r) where
  display (BG5Novem d1 sq2 g3 d4 sq5 sq6) = display d1
    . display sq2 . display g3 . display d4 . display sq5
    . display sq6
  display (BG5Zero z1 sq2 bg6'3) = display z1 . display sq2 . display bg6'3

data BG6 r
  = BG6Novem D9 (Seq D9z) r D9z (Seq D9z)
             (Seq (r, D9z, Seq D9z))
  | BG6Group r (BG7 r)
  deriving (Eq, Ord, Show)

instance Grouped BG6 where
  groupers (BG6Novem _ _ g1 _ _ sq)
    = NonEmpty g1 (fmap (\(x, _, _) -> x) sq)
  groupers (BG6Group g1 bg7) = NonEmpty g1 (mayGroupers bg7)

instance Display r => Display (BG6 r) where
  display (BG6Novem d1 sq2 r3 d4 sq5 sq6)
    = display d1 . display sq2 . display r3 . display d4
      . display sq5 . display sq6
  display (BG6Group g1 bg7'2) = display g1 . display bg7'2

data BG7 r
  = BG7Zeroes Zero (Seq Zero) (BG8 r)
  | BG7Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

instance Display r => Display (BG7 r) where
  display (BG7Zeroes z1 sq2 bg8'3) = display z1 . display sq2 . display bg8'3
  display (BG7Novem d1 sq2 sq3) = display d1 . display sq2 . display sq3

data BG8 r
  = BG8Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  | BG8Group r (BG7 r)
  deriving (Eq, Ord, Show)

instance MayGrouped BG7 where
  mayGroupers (BG7Zeroes _ _ bg8) = mayGroupers bg8
  mayGroupers (BG7Novem _ _ sq) = fmap (\(x, _, _) -> x) sq

instance MayGrouped BG8 where
  mayGroupers (BG8Novem _ _ sq) = fmap (\(x, _, _) -> x) sq
  mayGroupers (BG8Group g1 bg7) = g1 <| mayGroupers bg7

instance Display r => Display (BG8 r) where
  display (BG8Novem d1 ds2 sq3) = display d1 . display ds2 . display sq3
  display (BG8Group r bg7) = display r . display bg7

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

instance Display (NilOrBrimScalar RadCom) where
  display (NilOrBrimScalar ei) = either display display ei

instance Display (NilOrBrimScalar RadPer) where
  display (NilOrBrimScalar ei) = either display display ei

-- | Number types that may be neutral or non-neutral, with either a
-- comma or period radix.  Does not have a polarity.
newtype NilOrBrimScalarAnyRadix
  = NilOrBrimScalarAnyRadix (Either (NilOrBrimScalar RadCom)
                                    (NilOrBrimScalar RadPer))
  deriving (Eq, Ord, Show)

instance Display NilOrBrimScalarAnyRadix where
  display (NilOrBrimScalarAnyRadix ei) = either display display ei

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

instance Display RepNonNeutralNoSide where
  display (RepNonNeutralNoSide ei) = either display display ei

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

instance MayGrouped QtyRep where
  mayGroupers (QtyRep (NilOrBrimPolar (Center n))) = mayGroupers n
  mayGroupers (QtyRep (NilOrBrimPolar (OffCenter o _))) = mayGroupers o

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

-- # Conversions

-- | Removes the 'Side' from a 'QtyRepAnyRadix'.
c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  :: QtyRepAnyRadix
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix (QtyRepAnyRadix ei)
  = either (stripper Left) (stripper Right) ei
  where
    stripper mkEi (QtyRep (NilOrBrimPolar coc)) = case coc of
      Center n -> NilOrBrimScalarAnyRadix (mkEi (NilOrBrimScalar (Left n)))
      OffCenter o _ -> NilOrBrimScalarAnyRadix
        (mkEi (NilOrBrimScalar (Right o)))

c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide
  :: RepNonNeutralNoSide
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide (RepNonNeutralNoSide ei)
  = either (create Left) (create Right) ei
  where
    create mkEi br = NilOrBrimScalarAnyRadix
      (mkEi (NilOrBrimScalar (Right br)))
