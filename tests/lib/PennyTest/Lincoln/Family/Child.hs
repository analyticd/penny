module PennyTest.Lincoln.Family.Child where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.Family.Child as C
import qualified Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary)

instance (Arbitrary c, Arbitrary p) => Arbitrary (C.Child p c) where
  arbitrary = C.Child <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary
