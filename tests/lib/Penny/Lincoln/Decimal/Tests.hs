{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Lincoln.Decimal.Tests where

import Test.QuickCheck
import Penny.Lincoln.Digits
import Penny.Lincoln.Decimal
import Penny.Lincoln.Natural
import Penny.Lincoln.Natural.Tests

newtype DecimalA = DecimalA Decimal
  deriving (Eq, Ord, Show)

instance Arbitrary DecimalA where
  arbitrary = do
    i <- arbitrary
    UnsignedA e <- arbitrary
    return . DecimalA $ Decimal i e

-- | Decimals with significands of zero.
newtype DecimalZero = DecimalZero Decimal
  deriving (Eq, Ord, Show)

instance Arbitrary DecimalZero where
  arbitrary = do
    UnsignedA e <- arbitrary
    return . DecimalZero $ Decimal 0 e

-- | Decimals equivalent to one.
newtype DecimalOne = DecimalOne Decimal
  deriving (Eq, Ord, Show)

instance Arbitrary DecimalOne where
  arbitrary = do
    expt <- choose (0, 15)
    let signif = 1 * 10 ^ expt
        e = maybe (error "DecimalOne: error") id . integerToNatural
          $ expt
    return . DecimalOne $ Decimal signif e

prop_addIsAssociative (DecimalA x) (DecimalA y) (DecimalA z) =
  x + (y + z) === (x + y) + z

prop_addIsCommutative (DecimalA x) (DecimalA y) =
  x + y === y + x

prop_xPlusYMinusY (DecimalA x) (DecimalA y) =
  Semantic (x + y - y) === Semantic x

prop_addLeftZero (DecimalZero x) (DecimalA y) =
  Semantic (x + y) === Semantic y

prop_DecimalOneEquivalentToOne (DecimalOne d1) =
  Semantic d1 === Semantic (Decimal 1 (fromDecem D0))

prop_DecimalZeroEquivalentToZero (DecimalZero d0) =
  Semantic d0 === Semantic (Decimal 0 (fromDecem D0))

prop_multiplyIsAssociative (DecimalA x) (DecimalA y) (DecimalA z) =
  x * (y * z) === (x * y) * z

prop_multiplyIsCommutative (DecimalA x) (DecimalA y) =
  x * y === y * x

prop_multiplyLeftDecimalOne (DecimalOne x) (DecimalA y) =
  Semantic (x * y) === Semantic y

prop_multiplyByZeroIsZero (DecimalZero x) (DecimalA y) =
  Semantic (x * y) === Semantic (Decimal 0 (fromDecem D0))

prop_increaseExponentExponentIsCorrect
  (UnsignedA u) (DecimalA d@(Decimal _ e))
  | e >= u = e' == e
  | otherwise = e' == u
  where
    Decimal _ e' = increaseExponent u d

prop_increaseExponentIsEquivIfIncreased
  (UnsignedA u) (DecimalA d@(Decimal _ e))
  | e >= u = d === d'
  | otherwise = Semantic d' === Semantic d
  where
    d' = increaseExponent u d

-- equalizeExponents

data EqualizedExponents
  = EqualizedExponents (Decimal, Decimal) (Decimal, Decimal)
  -- ^ @EqualizedExponents a b@, where
  --
  -- @a@ is the original pair of decimals
  -- @b@ is the equalized pair of decimals
  deriving (Eq, Ord, Show)

instance Arbitrary EqualizedExponents where
  arbitrary = do
    DecimalA x <- arbitrary
    DecimalA y <- arbitrary
    return $ EqualizedExponents (x, y) (equalizeExponents x y)

prop_equalizeExponentsChangesOnlyOneOperand
  (EqualizedExponents (x, y) (x', y')) = x === x' .||. y === y'

prop_equalizeExponentsSameExponent
  (EqualizedExponents _ (Decimal _ ex, Decimal _ ey)) = ex === ey

prop_equalizeExponentsOnlyIncreasesExponent
  (EqualizedExponents (Decimal _ ex, Decimal _ ey)
                      (Decimal _ ex', Decimal _ ey'))
  = ex' >= ex && ey' >= ey

prop_equalizeExponentOnlyIncreasesSignificandAbs
  (EqualizedExponents (Decimal sx _, Decimal sy _)
                      (Decimal sx' _, Decimal sy' _))
  = abs sx' >= abs sx && abs sy' >= abs sy

prop_equalizeExponentsLeftResultEquivalent
  (EqualizedExponents (x, _) (x', _)) = Semantic x === Semantic x'

prop_equalizeExponentsRightResultEquivalent
  (EqualizedExponents (_, y) (_, y')) = Semantic y === Semantic y'

prop_doubleNegate (DecimalA x) = negate (negate x) === x

prop_absAndSignum (DecimalA x) = abs x * signum x === x

prop_fromIntegerNoExponent int =
  let Decimal _ ex = fromInteger int
  in ex === fromDecem D0

prop_fromIntegerSameSignificand int =
  let Decimal signif _ = fromInteger int
  in signif === int
