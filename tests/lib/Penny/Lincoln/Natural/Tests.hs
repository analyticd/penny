{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Lincoln.Natural.Tests where

import Test.QuickCheck hiding (NonZero)
import qualified Test.QuickCheck as Q
import Penny.Lincoln.Natural

newtype NonZeroA = NonZeroA NonZero
  deriving (Eq, Ord, Show)

instance Arbitrary NonZeroA where
  arbitrary = do
    Q.NonZero i <- arbitrary
    case integerToNatural (abs i) of
      Nothing -> fail "could not generate NonZeroA"
      Just n -> return $ NonZeroA n

newtype UnsignedA = UnsignedA Unsigned
  deriving (Eq, Ord, Show)

instance Arbitrary UnsignedA where
  arbitrary = do
    i <- arbitrary
    case integerToNatural (abs i) of
      Nothing -> fail "could not generate UnsignedA"
      Just n -> return $ UnsignedA n

nextPrevGivesSameNumber :: (Eq a, Natural a, Show a) => a -> Property
nextPrevGivesSameNumber a = case prev . next $ a of
  Nothing -> property False
  Just r -> a === r

prop_nextPrevGivesSameNumberUnsigned (UnsignedA x)
  = nextPrevGivesSameNumber x

prop_nextPrevGivesSameNumberNonZero (NonZeroA x)
  = nextPrevGivesSameNumber x

addIsAssociative :: (Eq a, Natural a, Show a) => a -> a -> a -> Property
addIsAssociative x y z =
  (x `add` y) `add` z === x `add` (y `add` z)

prop_addIsAssociativeUnsigned (UnsignedA x) (UnsignedA y) (UnsignedA z) =
  addIsAssociative x y z

prop_addIsAssociativeNonZero (NonZeroA x) (NonZeroA y) (NonZeroA z) =
  addIsAssociative x y z

addIsCommutative :: (Eq a, Natural a, Show a) => a -> a -> Property
addIsCommutative x y = x `add` y === y `add` x

multIsAssociative :: (Eq a, Natural a, Show a) => a -> a -> a -> Property
multIsAssociative x y z =
  (x `mult` y) `mult` z === x `mult` (y `mult` z)

prop_multIsAssociativeUnsigned (UnsignedA x) (UnsignedA y) (UnsignedA z)
  = multIsAssociative x y z

prop_multIsAssociativeNonZero (NonZeroA x) (NonZeroA y) (NonZeroA z)
  = multIsAssociative x y z

prop_monusReturnsValidUnsigned (UnsignedA x) (UnsignedA y) =
  let r = x `monus` y
  in naturalToInteger r >= 0
