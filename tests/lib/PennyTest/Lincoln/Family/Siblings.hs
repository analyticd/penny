module PennyTest.Lincoln.Family.Siblings where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Penny.Lincoln.Family.Siblings as S
import Test.QuickCheck (Gen, listOf, Arbitrary(arbitrary),
                        Property, printTestCase)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- | Generate Siblings. The length of the tail depends on the size
-- parameter.
genSiblings :: Gen a -> Gen a -> Gen a -> Gen (S.Siblings a)
genSiblings g1 g2 gr =
  S.Siblings
  <$> g1
  <*> g2
  <*> listOf gr

newtype AnySiblings a =
  AnySiblings (S.Siblings a)
  deriving (Show, Eq)
instance Arbitrary a => Arbitrary (AnySiblings a) where
  arbitrary = AnySiblings
              <$> genSiblings arbitrary arbitrary arbitrary

-- | Generate a NonEmpty list. The size of the tail depends upon the
-- size parameter.
genNonEmpty :: Gen a -> Gen a -> Gen (NE.NonEmpty a)
genNonEmpty g1 gr =
  (:|) <$> g1 <*> listOf gr

-- | Collapsing Siblings does not change the number of elements.
prop_numCollapse :: Property
prop_numCollapse = do
  let g = genNonEmpty (pure ()) (pure ())
  s <- genSiblings g g g
  let count = length . concat . F.toList . fmap F.toList $ s
      collapsed = S.collapse s
      count' = length . F.toList $ collapsed
      err = show s ++ " length of original: " ++ show count
            ++ " length of new: " ++ show count' ++ " new: "
            ++ show collapsed
  printTestCase err (count == count')

test_numCollapse :: Test
test_numCollapse = testProperty s prop_numCollapse where
  s = "Collapsing Siblings does not change number of elements"

tests :: Test
tests = testGroup "Siblings"
        [ test_numCollapse ]
