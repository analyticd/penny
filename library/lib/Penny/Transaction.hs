module Penny.Transaction where

import Penny.TopLine
import Penny.Posting
import Penny.Balanced
import Data.Sequence (Seq)

data Transaction = Transaction
  { txnTopLine :: TopLine
  , txnEnts :: Balanced Posting
  } deriving (Eq, Ord, Show)

data Bundle = Bundle
  { bunTopLine :: TopLine
  , bunView :: View Posting
  } deriving (Eq, Ord, Show)

bundles :: Transaction -> Seq Bundle
bundles (Transaction tl es) = fmap mkBundle . allViews $ es
  where
    mkBundle v = Bundle tl v

bundleToTransaction :: Bundle -> Transaction
bundleToTransaction (Bundle tl v) = Transaction tl (unView v)
