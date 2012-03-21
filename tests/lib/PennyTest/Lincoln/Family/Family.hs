module PennyTest.Lincoln.Family.Family where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.Family.Family as F
import Test.QuickCheck (Gen, listOf)

genFamily :: Gen p -> Gen c -> Gen c -> Gen c -> Gen (F.Family p c)
genFamily gp gc1 gc2 gcr =
  F.Family
  <$> gp
  <*> gc1
  <*> gc2
  <*> listOf gcr
