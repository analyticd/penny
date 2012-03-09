module PennyTest.Lincoln.Family.Siblings where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.Family.Siblings as S
import qualified Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary a => Arbitrary (S.Siblings a) where
  arbitrary = S.Siblings <$> arbitrary <*> arbitrary <*> arbitrary
