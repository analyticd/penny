module PennyTest.Lincoln.Transaction.Unverified where

import Control.Applicative ((<$>), (<*>))
import PennyTest.Lincoln.Bits ()
import qualified Penny.Lincoln.Transaction.Unverified as U
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary U.TopLine where
  arbitrary = U.TopLine <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary <*> arbitrary

instance Arbitrary U.Posting where
  arbitrary = U.Posting <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
