{-# OPTIONS_GHC -fno-warn-orphans #-}
module Penny.Lincoln.Instances where

import Control.Applicative
import Penny.Lincoln
import Control.Monad
import Test.QuickCheck hiding (Positive, NonZero)
import qualified Test.QuickCheck as Q
import qualified Data.Text as X
import qualified Data.Map as M
import Data.Time
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.DeriveTH

------------------------------------------------------------
-- From Other Packages
------------------------------------------------------------

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = Seq.fromList <$> arbitrary

-- Balances
instance Arbitrary Balances where
  arbitrary = Balances <$> (M.fromList <$> arbitrary)

instance Arbitrary Imbalances where
  arbitrary = Imbalances <$> (M.fromList <$> arbitrary)

-- Commodity

instance Arbitrary Commodity where
  arbitrary = Commodity <$> (X.pack <$> arbitrary)

-- DateTime

instance Arbitrary Date where
  arbitrary = Date <$> (ModifiedJulianDay <$> arbitrary)

instance Arbitrary DateTime where
  arbitrary = DateTime
    <$> arbitrary
    <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary Minutes where
  arbitrary = Minutes <$> arbitrary

instance Arbitrary Seconds where
  arbitrary = Seconds <$> arbitrary

instance Arbitrary Hours where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Enum60 where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Zone where
  arbitrary = do
    i <- choose (-2399, 2399)
    case intToZone i of
      Nothing -> fail "could not generate Zone"
      Just r -> return r

-- Decimal

instance Arbitrary Decimal where
  arbitrary = liftM2 Decimal arbitrary arbitrary

instance Arbitrary Semantic where
  arbitrary = Semantic <$> arbitrary

instance Arbitrary DecNonZero where
  arbitrary = liftM2 DecNonZero arbitrary arbitrary

instance Arbitrary DecUnsigned where
  arbitrary = liftM2 DecUnsigned arbitrary arbitrary

instance Arbitrary DecPositive where
  arbitrary = liftM2 DecPositive arbitrary arbitrary

-- Ent

instance Arbitrary a => Arbitrary (Ent a) where
  arbitrary = Ent <$> arbitrary <*> arbitrary <*> arbitrary

-- Ents - TODO still need Balanced

instance Arbitrary a => Arbitrary (Ents a) where
  arbitrary = sized $ \s -> do
    len <- choose (0, s)
    go len
    where
      go sz
        | sz == 0 = return mempty
        | otherwise = do
            ent <- arbitrary
            rest <- go (sz - 1)
            return $ prependEnt ent rest

-- Natural
instance Arbitrary Positive where
  arbitrary = do
    Q.Positive i <- arbitrary
    case integerToNatural i of
      Nothing -> fail "could not create Positive."
      Just r -> return r

instance Arbitrary Unsigned where
  arbitrary = do
    Q.NonNegative i <- arbitrary
    case integerToNatural i of
      Nothing -> fail "could not generate NonNegative."
      Just r -> return r

-- NonZero
instance Arbitrary NonZero where
  arbitrary = do
    Q.NonZero i <- arbitrary
    case integerToNonZero i of
      Nothing -> fail "could not generate NonZero."
      Just r -> return r

-- PluMin
$(derive makeArbitrary ''PluMin)

-- Qty
instance Arbitrary Qty where
  arbitrary = fmap Qty arbitrary

instance Arbitrary QtyNonZero where
  arbitrary = fmap QtyNonZero arbitrary

instance Arbitrary QtyUnsigned where
  arbitrary = fmap QtyUnsigned arbitrary

-- Rep
instance Arbitrary (Radix a) where
  arbitrary = return Radix

instance Arbitrary Grouper where
  arbitrary = elements [ ThinSpace, Underscore, GrSpace ]

instance Arbitrary RadCom where
  arbitrary = oneof [ return Period, RCGrouper <$> arbitrary ]

instance Arbitrary RadPer where
  arbitrary = oneof [ return Comma, RPGrouper <$> arbitrary ]

instance Arbitrary Zero where arbitrary = return Zero

instance Arbitrary r => Arbitrary (Nil r) where
  arbitrary = oneof [ NilU <$> arbitrary, NilG <$> arbitrary ]

instance Arbitrary r => Arbitrary (NilGrouped r) where
  arbitrary = NilGrouped <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary
    <*> arbitrary

$(derive makeArbitrary ''NilUngrouped )
$(derive makeArbitrary ''Brim)
$(derive makeArbitrary ''BrimGrouped)
$(derive makeArbitrary ''BrimUngrouped)
$(derive makeArbitrary ''BG1)
$(derive makeArbitrary ''BG5)
$(derive makeArbitrary ''BG6)
$(derive makeArbitrary ''BG7)
$(derive makeArbitrary ''BG8)
$(derive makeArbitrary ''CenterOrOffCenter)
$(derive makeArbitrary ''NilOrBrimPolar)
$(derive makeArbitrary ''NilOrBrimScalar)
$(derive makeArbitrary ''NilOrBrimScalarAnyRadix)
$(derive makeArbitrary ''RepNonNeutralNoSide)
$(derive makeArbitrary ''QtyRep)
$(derive makeArbitrary ''QtyRepAnyRadix)
$(derive makeArbitrary ''ExchRep)
$(derive makeArbitrary ''ExchRepAnyRadix)

-- Rep.Digits
$(derive makeArbitrary ''Novem)
$(derive makeArbitrary ''Decem)

-- Side
$(derive makeArbitrary ''Side)
