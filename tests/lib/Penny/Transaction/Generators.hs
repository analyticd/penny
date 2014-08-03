module Penny.Transaction.Generators where

import Penny.Transaction
import Test.QuickCheck
import Penny.TopLine.Generators
import Penny.Posting.Generators
import qualified Data.Sequence as S
import Control.Monad
import Penny.Balanced
import qualified Penny.Balanced.Generators as G

transaction :: Gen Transaction
transaction = liftM2 Transaction topLine (G.balanced posting)

bundle :: Gen Bundle
bundle = transaction `suchThat` pd >>= f
  where
    pd = not . S.null . balancedEnts . txnEnts
    f txn = do
      let sq = bundles txn
      ix <- choose (0, S.length sq)
      return $ S.index sq ix
