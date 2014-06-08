module Penny.Lincoln.Transaction where

import Penny.Lincoln.TopLine
import Penny.Lincoln.Posting
import Penny.Lincoln.Ents

data Transaction = Transaction
  { txnTopLine :: TopLine
  , txnEnts :: Ents Posting
  } deriving (Eq, Ord, Show)

data Bundle = Bundle
  { bunTopLine :: TopLine
  , bunView :: View Posting
  } deriving (Eq, Ord, Show)

bundles :: Transaction -> [Bundle]
bundles (Transaction tl es) = map mkBundle . allViews $ es
  where
    mkBundle v = Bundle tl v

bundleToTransaction :: Bundle -> Transaction
bundleToTransaction (Bundle tl v) = Transaction tl (unView v)
