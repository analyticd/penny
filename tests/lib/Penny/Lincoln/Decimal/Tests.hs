module Penny.Lincoln.Decimal.Tests where

import Test.QuickCheck
import Penny.Lincoln.Decimal
import Penny.Lincoln.Natural.Tests

newtype DecimalA = DecimalA Decimal
  deriving (Eq, Ord, Show)

instance Arbitrary DecimalA where
  arbitrary = do
    i <- arbitrary
    UnsignedA e <- arbitrary
    return . DecimalA $ Decimal i e

prop_addIsAssociative (DecimalA x) (DecimalA y) (DecimalA z) =
  x `add` (y `add` z) === (x `add` y) `add` z

prop_addIsCommutative (DecimalA x) (DecimalA y) =
  x `add` y === y `add` x

prop_xPlusYMinusY (DecimalA x) (DecimalA y) =
  (x `add
