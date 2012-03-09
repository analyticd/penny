module PennyTest.Lincoln.Family where

import qualified Data.Foldable as Foldable

import qualified Penny.Lincoln.Family as TF
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S
import PennyTest.Lincoln.Family.Family ()
import PennyTest.Lincoln.Family.Siblings ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as T

-- | A family broken into children has as many children as the family
-- had.
prop_childCount :: F.Family () () -> Bool
prop_childCount f = expected == actual where
  expected = length . F.children $ f
  actual = length . S.rest . TF.children $ f

test_childCount :: T.Test
test_childCount =
  testProperty "Number of children from children function"
  prop_childCount

-- | Each sibling in a family broken into children has as many
-- siblings as the family had.
prop_siblingCount :: F.Family () () -> Bool
prop_siblingCount f = Foldable.all p s where
  s = TF.children f
  p c = (length . C.siblings $ c)
        == (length . F.children $ f)

test_siblingCount :: T.Test
test_siblingCount = testProperty s prop_siblingCount where
  s = "Number of siblings in children from children function"

-- | The number of orphans from a family has as many children as the
-- family had.
prop_orphanCount :: F.Family () () -> Bool
prop_orphanCount f = expected == actual where
  expected = length . F.children $ f
  actual = length . S.rest . TF.orphans $ f

test_orphanCount :: T.Test
test_orphanCount =
  testProperty "Number of orphans from orphan function"
  prop_orphanCount

-- | The number of children in an adopted family stays the same.
prop_adoptedCount :: S.Siblings () -> Bool
prop_adoptedCount s = expected == actual where
  expected = length . S.rest $ s
  actual = length . F.children . TF.adopt () $ s

test_adoptedCount :: T.Test
test_adoptedCount =
  testProperty "Number of children in an adopted family"
  prop_adoptedCount

-- | The size of a married family is as large as the smaller marrying
-- family.
prop_marriedSize :: (F.Family () (), F.Family () ()) -> Bool
prop_marriedSize (f1, f2) = expected == actual where
  len = length . F.children
  expected = min (len f1) (len f2)
  actual = len (TF.marry f1 f2)

test_marriedSize :: T.Test
test_marriedSize =
  testProperty "Size of a married family" prop_marriedSize

-- | The size of both divorced families is the same as the divorcing
-- family.
prop_divorcedSize :: F.Family ((), ()) ((), ()) -> Bool
prop_divorcedSize f = len d1 == expected && len d2 == expected where
  len = length . F.children
  expected = len f
  (d1, d2) = TF.divorce f

test_divorcedSize :: T.Test
test_divorcedSize = testProperty s prop_divorcedSize where
  s = "Size of divorced families"

tests :: T.Test
tests = T.testGroup "Family"
        [test_childCount, test_siblingCount, test_orphanCount,
         test_adoptedCount, test_marriedSize, test_divorcedSize]
