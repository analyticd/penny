module Penny.Lincoln.Decimal where

import Penny.Lincoln.Natural
import Penny.Lincoln.Rep.Digits

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

data DecUnsigned
  = DecUnsigned !Unsigned !Unsigned
  -- ^ @DecUnsigned a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent
  deriving (Eq, Ord, Show)

