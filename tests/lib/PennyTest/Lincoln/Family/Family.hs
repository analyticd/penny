module PennyTest.Lincoln.Family.Family where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.Family.Family as F
import qualified Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary)

instance (Arbitrary p, Arbitrary c) => Arbitrary (F.Family p c) where
  arbitrary = F.Family <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary
