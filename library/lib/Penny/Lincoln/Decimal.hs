module Penny.Lincoln.Decimal where

import Penny.Lincoln.Natural
import qualified Penny.Lincoln.Natural as N
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits
import Control.Monad (join)
import Data.Sequence ((<|))
import Data.Monoid

-- | Decimal numbers.  The precision is limited only by the machine's
-- available memory (or, more realistically, by how big a number the
-- machine can handle before grinding to unusable slowness.)  The 'Eq'
-- and 'Ord' instances are derived; therefore:
--
-- >>> let twoPointZero = Decimal 20 . fromNovem $ D1
-- >>> let twoPointZeroZero = Decimal 200 . fromNovem $ D2
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
  -- > Decimal 200 . fromNovem $ D2
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
  signum (Decimal mx _) = Decimal (signum mx) (fromDecem D0)
  fromInteger i = Decimal i (fromDecem D0)

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

instance HasDecimal (NilUngrouped r) where
  toDecimal nu = Decimal 0 expt
    where
      expt = case nu of
        NUZero _ Nothing -> fromDecem D0
        NUZero _ (Just (_, Nothing)) -> fromDecem D0
        NUZero _ (Just (_, Just (_, zs))) -> next (N.length zs)
        NURadix (_, _, zs) -> next (N.length zs)

instance HasDecimal (Nil r) where
  toDecimal nil = case nil of
    NilUngrouped nu -> toDecimal nu
    NilGrouped _ _ _ zs1 _ _ zs2 zss -> Decimal 0
      . next . next . add (N.length zs1) . add (N.length zs2)
      . N.length . join
      . fmap (\(_, _, sq) -> Zero <| sq) $ zss

class HasDecPositive a where
  toDecPositive :: a -> DecPositive

instance HasDecPositive (BrimUngrouped r) where

  toDecPositive (BUGreaterThanOne nv ds1 Nothing)
    = DecPositive (novDecsToPositive nv ds1) (fromDecem D0)

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
      expt = fromDecem D0

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss
    (Just (_, Nothing)))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . join . fmap (\(_, d, ds) -> d <| ds) $ dss
      expt = fromDecem D0

  toDecPositive (BGGreaterThanOne nv ds1
    (BG1GroupOnLeft _ d1 ds2 dss1
    (Just (_, Just (d2, ds3, dss2))))) = DecPositive sig expt
    where
      sig = novDecsToPositive nv . (ds1 <>) . (d1 <|) . (ds2 <>)
        . (toDecs dss1 <>) . (d2 <|) . (ds3 <>) . toDecs $ dss2
      expt = next . add (N.length ds3) . N.length . toDecs $ dss2
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)

  toDecPositive (BGGreaterThanOne nv1 ds2
    (BG1GroupOnRight _rdx3 d4 ds5 dss6)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv1 . (ds2 <>) . (d4 <|) . (ds5 <>)
        . toDecs $ dss6
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (N.length ds5) . N.length . toDecs $ dss6

  toDecPositive (BGLessThanOne _z1 _rdx2
    (BG5Novem nv3 ds4 _g5 d6 ds7 sq8)) = DecPositive sig expt
    where
      sig = novDecsToPositive nv3 . (ds4 <>) . (d6 <|) . (ds7 <>)
        . toDecs $ sq8
      toDecs = join . fmap (\(_, d, ds) -> d <| ds)
      expt = next . add (N.length ds4) . next . add (N.length ds7)
        . N.length . toDecs $ sq8

