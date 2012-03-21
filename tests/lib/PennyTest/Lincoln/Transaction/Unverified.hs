-- | Orphan instances for Arbitrary for unverified TopLine and for
-- unverified Posting.
module PennyTest.Lincoln.Transaction.Unverified where

import Control.Applicative ((<$>), (<*>))
import qualified PennyTest.Lincoln.Bits as TB
import qualified Penny.Lincoln.Transaction.Unverified as U
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary U.TopLine where
  arbitrary = U.TopLine <$> TB.genDateTime <*> arbitrary <*> arbitrary
              <*> arbitrary <*> arbitrary

instance Arbitrary U.Posting where
  arbitrary = U.Posting <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
