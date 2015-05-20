module Penny.Transbox.Matcher where

import Data.Sequence (Seq)
import Penny.Ledger
import Penny.Matcher
import Penny.Transbox

transactionMeta
  :: Ledger l
  => Matcher (Seq (TreeL l)) l r
  -> Matcher (Transbox l a) l r
transactionMeta mr
  = tunnel (fmap return _transaction)
  `feed` tunnel txnMeta
  `feed` mr
