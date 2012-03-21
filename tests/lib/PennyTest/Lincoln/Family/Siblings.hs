module PennyTest.Lincoln.Family.Siblings where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.Family.Siblings as S
import Test.QuickCheck (Gen, listOf)

-- | Generate Siblings. The length of the tail depends on the size
-- parameter.
genSiblings :: Gen a -> Gen a -> Gen a -> Gen (S.Siblings a)
genSiblings g1 g2 gr =
  S.Siblings
  <$> g1
  <*> g2
  <*> listOf gr
