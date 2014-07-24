module Penny.Transaction where

import Penny.TopLine
import Penny.Posting
import Penny.Ents

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
