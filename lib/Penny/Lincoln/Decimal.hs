module Penny.Lincoln.Decimal where

import Penny.Lincoln.Natural
import qualified Penny.Lincoln.Natural as N
import Penny.Lincoln.NonZero
import Penny.Lincoln.Rep
import Control.Monad (join)
import Data.Sequence ((<|), (|>), Seq)
import qualified Data.Sequence as S
import Data.Monoid
import Penny.Lincoln.Offset

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
  signum (Decimal mx _) = Decimal (signum mx) (toUnsigned D0z'0)
  fromInteger i = Decimal i (toUnsigned D0z'0)

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

-- | Class for things that can be converted to a 'Decimal'.
class HasDecimal a where
  toDecimal :: a -> Decimal

class HasExponent a where
  toExponent :: a -> Unsigned

instance HasDecimal DecPositive where
  toDecimal (DecPositive sig expt) = Decimal (naturalToInteger sig) expt

instance HasExponent (NilUngrouped r) where
  toExponent nu = case nu of
    NUZero _ Nothing -> toUnsigned D0z'0
    NUZero _ (Just (_, Nothing)) -> toUnsigned D0z'0
    NUZero _ (Just (_, Just (_, zs))) -> next (N.length zs)
    NURadix _ _ zs -> next (N.length zs)

instance HasExponent (Nil r) where
  toExponent nil = case nil of
    NilU nu -> toExponent nu
    NilG ng -> toExponent ng

instance HasExponent (NilGrouped r) where
  toExponent (NilGrouped _ _ _ zs1 _ _ zs2 zss) =
      next . next . add (N.length zs1) . add (N.length zs2)
      . N.length . join
      . fmap (\(_, _, sq) -> Zero <| sq) $ zss

class HasDecPositive a where
  toDecPositive :: a -> DecPositive

-- | Strips the sign from the 'DecNonZero'.
instance HasDecPositive DecNonZero where
  toDecPositive (DecNonZero sig expt) =
    DecPositive (nonZeroToPositive sig) expt

instance HasDecPositive (BrimUngrouped r) where

  toDecPositive (BUGreaterThanOne nv ds1 Nothing)
    = DecPositive (novDecsToPositive nv ds1) (toUnsigned D0z'0)

  toDecPositive (BUGreaterThanOne nv ds1 (Just (_, ds2)))
    = DecPositive (novDecsToPositive nv (ds1 <> ds2))
                  (N.length ds2)

  toDecPositive (BULessThanOne _ _ zs1 nv ds)
    = DecPositive (novDecsToPositive nv ds)
                  (add (N.length zs1) . next . N.length $ ds)

instance HasDecPositive (BrimGrouped r) where

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss Nothing))
    = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . join . fmap (\(_, d, ds) -> d <| ds) $ dss
      expt = toUnsigned D0z'0

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss
    (Just (_, Nothing)))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . join . fmap (\(_, d, ds) -> d <| ds) $ dss
      expt = toUnsigned D0z'0

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss1
    (Just (_, Just (d2, ds3, dss2))))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . (toDecs dss1 <>) . (d2 <|) . (ds3 <>) . toDecs $ dss2
      expt = next . add (N.length ds3) . N.length . toDecs $ dss2
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)

  toDecPositive (BGGreaterThanOne nv1 ds2
    (BG1GroupOnRight _rdx3 d4 ds5 _g6 d7 ds8 dss9)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv1 . (ds2 <>) . (d4 <|) . (ds5 <>)
        . (d7 <|) . (ds8 <>)
        . toDecs $ dss9
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (N.length ds5) . next . add (N.length ds8)
        . N.length . toDecs $ dss9

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Novem nv3 ds4 _g5 d6 ds7 sq8)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv3 . (ds4 <>) . (d6 <|) . (ds7 <>)
        . toDecs $ sq8
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (N.length ds4) . next . add (N.length ds7)
        . N.length . toDecs $ sq8

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Zero _z3 zs4 (BG6Novem nv5 ds6 _g7 dc8 ds9 sq10)))
    = DecPositive sig expt
    where
      sig = novDecsToPositive nv5 . (ds6 <>) . (dc8 <|) . (<> ds9)
        . toDecs $ sq10
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (N.length zs4) . next . add (N.length ds6)
        . next . add (N.length ds9) . N.length . toDecs $ sq10

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Zero _z3 zs4 (BG6Group _g1 bg7))) = DecPositive sig expt
    where
      sig = novDecsToPositive bg7nv bg7ds
      expt = next . add (N.length zs4) . N.length $ bg7zs
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

