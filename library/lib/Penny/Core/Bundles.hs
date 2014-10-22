module Penny.Core.Bundles where

import qualified Penny.Core.Bundle as Bundle
import qualified Penny.Core.Bundle.Internal as Bundle.Internal
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Core.View as View
import qualified Penny.Core.Balanced as Balanced
import Data.Sequence (Seq)

newtype T = T { toBundleSeq :: Seq Bundle.T }
  deriving (Eq, Ord, Show)

fromTransaction :: Transaction.T -> T
fromTransaction (Transaction.T tl bal)
  = T
  . fmap (Bundle.Internal.T tl)
  . View.allViews
  . Balanced.toSeq
  $ bal

