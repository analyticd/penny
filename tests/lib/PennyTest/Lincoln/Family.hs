module PennyTest.Lincoln.Family where

import Control.Applicative ((<$>))
import qualified Data.Foldable as Foldable

import qualified Penny.Lincoln.Family as TF
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S
import qualified PennyTest.Lincoln.Family.Family as TF
import qualified PennyTest.Lincoln.Family.Siblings as TS
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (
  property, Property, Gen, Arbitrary(arbitrary))
import qualified Test.Framework as T

genUnitFamily :: Gen (F.Family () ())
genUnitFamily = TF.genFamily e e e e where
  e = return ()

newtype UnitFamily = UnitFamily (F.Family () ())
                     deriving (Eq, Show)
instance Arbitrary UnitFamily where
  arbitrary = UnitFamily <$> genUnitFamily

-- | A family broken into children has as many children as the family
-- had.
prop_childCount :: Property
prop_childCount = do
  f <- genUnitFamily
  let expected = length . F.children $ f
      actual = length . S.rest . TF.children $ f
  property (expected == actual)

test_childCount :: T.Test
test_childCount =
  testProperty "Number of children from children function"
  prop_childCount

-- | Each sibling in a family broken into children has as many
-- siblings as the family had.
prop_siblingCount :: UnitFamily -> Bool
prop_siblingCount (UnitFamily f) = Foldable.all p s where
  s = TF.children f
  p c = (length . C.siblings $ c)
        == (length . F.children $ f)

test_siblingCount :: T.Test
test_siblingCount = testProperty s prop_siblingCount where
  s = "Number of siblings in children from children function"

-- | The number of orphans from a family has as many children as the
-- family had.
prop_orphanCount :: UnitFamily -> Bool
prop_orphanCount (UnitFamily f) = expected == actual where
  expected = length . F.children $ f
  actual = length . S.rest . TF.orphans $ f

test_orphanCount :: T.Test
test_orphanCount =
  testProperty "Number of orphans from orphan function"
  prop_orphanCount

-- | The number of children in an adopted family stays the same.
prop_adoptedCount :: Property
prop_adoptedCount = do 
  s <- TS.genSiblings (return ()) (return ()) (return ())
  let expected = length . S.rest $ s
      actual = length . F.children . TF.adopt () $ s
  property (expected == actual)

test_adoptedCount :: T.Test
test_adoptedCount =
  testProperty "Number of children in an adopted family"
  prop_adoptedCount

-- | The size of a married family is as large as the smaller marrying
-- family.
prop_marriedSize :: UnitFamily -> UnitFamily -> Bool
prop_marriedSize (UnitFamily f1) (UnitFamily f2) =
  expected == actual where
    len = length . F.children
    expected = min (len f1) (len f2)
    actual = len (TF.marry f1 f2)

test_marriedSize :: T.Test
test_marriedSize =
  testProperty "Size of a married family" prop_marriedSize

newtype DoubleUnitFamily =
  DoubleUnitFamily (F.Family ((), ()) ((), ()))
  deriving (Eq, Show)
instance Arbitrary DoubleUnitFamily where
  arbitrary = DoubleUnitFamily <$> TF.genFamily e e e e where
    e = return ((), ())

-- | The size of both divorced families is the same as the divorcing
-- family.
prop_divorcedSize :: DoubleUnitFamily -> Bool
prop_divorcedSize (DoubleUnitFamily f) =
  len d1 == expected && len d2 == expected where
    len = length . F.children
    expected = len f
    (d1, d2) = TF.divorce f

test_divorcedSize :: T.Test
test_divorcedSize = testProperty s prop_divorcedSize where
  s = "Size of divorced families"

tests :: T.Test
tests = T.testGroup "Family"
        [test_childCount, test_siblingCount, test_orphanCount,
         test_adoptedCount, test_marriedSize, test_divorcedSize,
         TS.tests]
