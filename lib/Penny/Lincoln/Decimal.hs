module Penny.Lincoln.Decimal where

import Penny.Lincoln.Natural
import Penny.Lincoln.NonZero
import Penny.Lincoln.Rep
import Control.Monad (join)
import Data.Sequence ((<|), (|>), Seq, ViewR(..), ViewL(..))
import qualified Data.Sequence as S
import Data.Monoid
import Penny.Lincoln.Offset
import Penny.Lincoln.PluMin
import Data.List (genericSplitAt, genericReplicate)

-- | Decimal numbers.  The precision is limited only by the machine's
-- available memory (or, more realistically, by how big a number the
-- machine can handle before grinding to unusable slowness.)  The 'Eq'
-- and 'Ord' instances are derived; therefore:
--
-- >>> let twoPointZero = Decimal 20 . toPositive $ D9'1
-- >>> let twoPointZeroZero = Decimal 200 . toPositive $ D9'2
-- >>> twoPointZero == twoPointZeroZero
-- False
-- >>> twoPointZeroZero > twoPointZero
-- True

data Decimal
  = Decimal !Integer !Unsigned
  -- ^ @Decimal a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent, such that the value of the number is
  --
  -- > a * 10 ^ (-1 * naturalToInteger b)
  --
  -- For example, @2.00@ is equal to
  --
  -- > Decimal 200 . toPositive $ D9'2
  deriving (Eq, Ord, Show)


-- | @increaseExponent d e@ returns a 'Decimal' @d'@ whose exponent is
-- equal to @e@; if the exponent of @d@ is greater than or equal to
-- @e@, does nothing.
increaseExponent :: Unsigned -> Decimal -> Decimal
increaseExponent u (Decimal m e) = Decimal m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m * 10 ^ naturalToInteger diff, u)


-- | Equalizes the exponents on two decimals.
--
-- Let:
--
-- > Decimal mx ex = x
-- > Decimal my ey = y
-- > (x'@(Decimal mx' ex'), y'@(Decimal my' ey')) = equalizeExponents x y
--
-- Then the following properties hold:
--
-- > ex' == ey'
-- > x' == x || y' == y
-- > ex' >= ex
-- > ey' >= ey
-- > mx' >= mx
-- > my' >= my
equalizeExponents :: Decimal -> Decimal -> (Decimal, Decimal)
equalizeExponents x@(Decimal _ ex) y@(Decimal _ ey)
  | ex > ey = (x, increaseExponent ex y)
  | otherwise = (increaseExponent ey x, y)

instance Num Decimal where
  x + y = Decimal (mx' + my') ex
    where
      (Decimal mx' ex, Decimal my' _) = equalizeExponents x y
  x - y = Decimal (mx' - my') ex
    where
      (Decimal mx' ex, Decimal my' _) = equalizeExponents x y
  (Decimal mx ex) * (Decimal my ey) = Decimal (mx * my) (ex `add` ey)
  negate (Decimal mx ex) = Decimal (negate mx) ex
  abs (Decimal mx ex) = Decimal (abs mx) ex
  signum (Decimal mx _) = Decimal (signum mx) (toUnsigned Zero)
  fromInteger i = Decimal i (toUnsigned Zero)

-- | Compares 'Decimal' based on semantics rather than actual values;
-- does this by first equalizing exponents before performing
-- comparisons.
newtype Semantic = Semantic Decimal
  deriving Show

instance Eq Semantic where
  Semantic x == Semantic y =
    let (Decimal mx _, Decimal my _) = equalizeExponents x y
    in mx == my

instance Ord Semantic where
  compare (Semantic x) (Semantic y) =
    let (Decimal mx _, Decimal my _) = equalizeExponents x y
    in compare mx my

-- | Decimals whose significand is never zero.

data DecNonZero = DecNonZero !NonZero Unsigned
  deriving (Eq, Ord, Show)

instance HasOffset DecNonZero where
  offset (DecNonZero sig expt) = DecNonZero (offset sig) expt

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (DecNonZero nz u) = Decimal (nonZeroToInteger nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Decimal signif expt) = case integerToNonZero signif of
  Nothing -> Nothing
  Just nz -> Just $ DecNonZero nz expt


-- | Decimals that are unsigned; they may be zero.
data DecUnsigned
  = DecUnsigned !Unsigned !Unsigned
  -- ^ @DecUnsigned a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent
  deriving (Eq, Ord, Show)

-- | Decimals that are positive; they may not be zero.
data DecPositive
  = DecPositive !Positive !Unsigned
  -- ^ @DecPositive a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent
  deriving (Eq, Ord, Show)

-- | Decimals whose significand is always zero.
data DecZero
  = DecZero !Unsigned
  -- ^ @DecZero a@, where @a@ is the exponent.  The significand is
  -- always zero so it does not have a field.
  deriving (Eq, Ord, Show)


-- | Class for things that can be converted to a 'Decimal'.
class HasDecimal a where
  toDecimal :: a -> Decimal

class HasExponent a where
  toExponent :: a -> Unsigned

instance HasDecimal DecPositive where
  toDecimal (DecPositive sig expt) = Decimal (naturalToInteger sig) expt

instance HasExponent (NilUngrouped r) where
  toExponent nu = case nu of
    NUZero _ Nothing -> toUnsigned Zero
    NUZero _ (Just (_, Nothing)) -> toUnsigned Zero
    NUZero _ (Just (_, Just (_, zs))) -> next (lengthUnsigned zs)
    NURadix _ _ zs -> next (lengthUnsigned zs)

instance HasExponent (Nil r) where
  toExponent nil = case nil of
    NilU nu -> toExponent nu
    NilG ng -> toExponent ng

instance HasExponent (NilGrouped r) where
  toExponent (NilGrouped _ _ _ zs1 _ _ zs2 zss) =
      next . next . add (lengthUnsigned zs1) . add (lengthUnsigned zs2)
      . lengthUnsigned . join
      . fmap (\(_, _, sq) -> Zero <| sq) $ zss

class HasDecPositive a where
  toDecPositive :: a -> DecPositive

-- | Strips the sign from the 'DecNonZero'.
instance HasDecPositive DecNonZero where
  toDecPositive (DecNonZero sig expt) =
    DecPositive (nonZeroToPositive sig) expt

instance HasDecPositive (BrimUngrouped r) where

  toDecPositive (BUGreaterThanOne nv ds1 Nothing)
    = DecPositive (novDecsToPositive nv ds1) (toUnsigned Zero)

  toDecPositive (BUGreaterThanOne nv ds1 (Just (_, ds2)))
    = DecPositive (novDecsToPositive nv (ds1 <> ds2))
                  (lengthUnsigned ds2)

  toDecPositive (BULessThanOne _ _ zs1 nv ds)
    = DecPositive (novDecsToPositive nv ds)
                  (add (lengthUnsigned zs1) . next . lengthUnsigned $ ds)

instance HasDecPositive (BrimGrouped r) where

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss Nothing))
    = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . join . fmap (\(_, d, ds) -> d <| ds) $ dss
      expt = toUnsigned Zero

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss
    (Just (_, Nothing)))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . join . fmap (\(_, d, ds) -> d <| ds) $ dss
      expt = toUnsigned Zero

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss1
    (Just (_, Just (d2, ds3, dss2))))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . (toDecs dss1 <>) . (d2 <|) . (ds3 <>) . toDecs $ dss2
      expt = next . add (lengthUnsigned ds3) . lengthUnsigned . toDecs $ dss2
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)

  toDecPositive (BGGreaterThanOne nv1 ds2
    (BG1GroupOnRight _rdx3 d4 ds5 _g6 d7 ds8 dss9)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv1 . (ds2 <>) . (d4 <|) . (ds5 <>)
        . (d7 <|) . (ds8 <>)
        . toDecs $ dss9
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (lengthUnsigned ds5) . next . add (lengthUnsigned ds8)
        . lengthUnsigned . toDecs $ dss9

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Novem nv3 ds4 _g5 d6 ds7 sq8)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv3 . (ds4 <>) . (d6 <|) . (ds7 <>)
        . toDecs $ sq8
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (lengthUnsigned ds4) . next . add (lengthUnsigned ds7)
        . lengthUnsigned . toDecs $ sq8

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Zero _z3 zs4 (BG6Novem nv5 ds6 _g7 dc8 ds9 sq10)))
    = DecPositive sig expt
    where
      sig = novDecsToPositive nv5 . (ds6 <>) . (dc8 <|) . (<> ds9)
        . toDecs $ sq10
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (lengthUnsigned zs4) . next . add (lengthUnsigned ds6)
        . next . add (lengthUnsigned ds9) . lengthUnsigned . toDecs $ sq10

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Zero _z3 zs4 (BG6Group _g1 bg7))) = DecPositive sig expt
    where
      sig = novDecsToPositive bg7nv bg7ds
      expt = next . add (lengthUnsigned zs4) . lengthUnsigned $ bg7zs
      (bg7zs, bg7nv, bg7ds) = unfurlBG7 bg7

      unfurlBG7 :: BG7 r -> (Seq Zero, D9, Seq D9z)
      unfurlBG7 = goBG7 S.empty
        where
          goBG7 zsSoFar (BG7Zeroes z1 zs bg8) =
            goBG8 ((zsSoFar |> z1) <> zs) bg8
          goBG7 zsSoFar (BG7Novem nv ds sq) =
            (zsSoFar, nv, ds <> toDecs sq)
          toDecs = join . fmap (\(_, d, ds) -> d <| ds)
          goBG8 zsSoFar (BG8Novem nv ds sq) =
            (zsSoFar, nv, ds <> toDecs sq)
          goBG8 zsSoFar (BG8Group _ b7) = goBG7 zsSoFar b7

instance HasDecPositive (Brim a) where
  toDecPositive (BrimGrouped a) = toDecPositive a
  toDecPositive (BrimUngrouped a) = toDecPositive a

instance HasDecPositive RepNonNeutralNoSide where
  toDecPositive (RepNonNeutralNoSide ei) =
    either toDecPositive toDecPositive ei

-- * Representations

repUngroupedDecimal
  :: Radix r
  -> Decimal
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) PluMin
repUngroupedDecimal rdx d = case stripDecimalSign d of
  Left zero -> Center (repDecZero rdx zero)
  Right (pos, pm) -> OffCenter (repDecPositive rdx pos) pm

repUngroupedDecNonZero
  :: Radix r
  -> DecNonZero
  -> (BrimUngrouped r, PluMin)
repUngroupedDecNonZero rdx nz = (repDecPositive rdx dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecUnsigned
  :: Radix r
  -> DecUnsigned
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) ()
repUngroupedDecUnsigned rdx uns = case decomposeDecUnsigned uns of
  Left z -> Center (repDecZero rdx z)
  Right p -> OffCenter (repDecPositive rdx p) ()

-- Primitive grouping functions

stripDecimalSign
  :: Decimal
  -> Either DecZero (DecPositive, PluMin)
stripDecimalSign (Decimal m e) = case stripIntegerSign m of
  Nothing -> Left (DecZero e)
  Just (p, pm) -> Right (DecPositive p e, pm)

stripNonZeroSign
  :: DecNonZero
  -> (DecPositive, PluMin)
stripNonZeroSign (DecNonZero nz ex)
  = (DecPositive (nonZeroToPositive nz) ex, nonZeroSign nz)

decomposeDecUnsigned
  :: DecUnsigned
  -> Either DecZero DecPositive
decomposeDecUnsigned (DecUnsigned m e) = case unsignedToPositive m of
  Nothing -> Left (DecZero e)
  Just m' -> Right (DecPositive m' e)

repDecZero :: Radix r -> DecZero -> NilUngrouped r
repDecZero rdx (DecZero expt) = case unsignedToPositive expt of
  Nothing -> NUZero Zero Nothing
  Just pos -> NUZero Zero (Just (rdx, Just (Zero, rest)))
    where
      rest = case prev pos of
        Nothing -> S.empty
        Just ps -> go ps (S.singleton Zero)
      go ps acc = case prev ps of
        Nothing -> acc
        Just ps' -> go ps' (Zero <| acc)

repDecPositive :: Radix r -> DecPositive -> BrimUngrouped r
repDecPositive rdx (DecPositive sig expt)
  = repDigits rdx (positiveDigits sig) expt

-- Let t = number of trailing significand digits,
-- and e = size of exponent.
--
-- t >= e: decimal is greater than zero
-- t < e: decimal is less than zero.
-- e - t - 1 == number of leading fractional zeroes

{-

S  Significand
LS Length of significand
LM Length of significand - 1
E  Exponent
P  Representation
LL Number of digits to left of radix
LZ Number of leading zeroes on the right of radix
LR Number of digits to right of radix

S       LS LM E   P             LL  LZ  LR

12345   5  4  0   12345         5   0   0
12345   5  4  3   12.345        2   0   3
12345   5  4  5   0.12345       0   0   5
12345   5  4  7   0.0012345     0   2   5
1       1  0  0   1             1   0   0
1       1  0  3   0.001         0   2   1

There are two possible scenarios: (1) LL is positive.  In that case
LZ is always zero and LR might be zero or positive.  (2) LL is zero.
In that case LR is positive and LZ might be zero or positive.

-}

repDigits
  :: Radix r
  -> (D9, [D9z])
  -> Unsigned
  -> BrimUngrouped r
repDigits rdx (d1, dr) expt
  = case diffUnsigned (next $ lengthUnsigned dr) expt of
      Equal -> BULessThanOne (Just Zero) rdx S.empty d1 (S.fromList dr)
      LeftBiggerBy l -> BUGreaterThanOne d1 leftDigs rightDigs
        where
          (leftDigs, rightDigs) = case prev l of
            Nothing -> (S.empty, Nothing)
            Just c -> (S.fromList beg, Just (rdx, S.fromList end))
              where
                (beg, end) = genericSplitAt (naturalToInteger c) dr
      RightBiggerBy r -> BULessThanOne (Just Zero) rdx zs d1 (S.fromList dr)
        where
          zs = S.fromList . flip genericReplicate Zero
            . naturalToInteger $ r

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
groupUngrouped
  :: r
  -> BrimUngrouped r
  -> Maybe (BrimGrouped r)
groupUngrouped _ (BULessThanOne _ _ _ _ _) = Nothing
groupUngrouped grpr (BUGreaterThanOne d1 ds mayAfter) =
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
