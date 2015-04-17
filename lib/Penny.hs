-- | This module is intended to form the basis of a REPL session.
--
-- Please see "Penny.Docs" for full documentation; the Haddocks in
-- this module are intended as a quick reference.
module Penny where

import Data.Monoid
import Data.Sequence (Seq)
import Penny.Amount
import Penny.Clatch
import Penny.Matcher
import Penny.Ledger
import Penny.SeqUtil
import qualified Data.Foldable as F
import Pipes
import Data.ByteString (ByteString)

data ClatchOptions l = ClatchOptions
  { converter :: Amount -> Maybe Amount
  , preFilter :: Matcher (TransactionL l, View (Converted (PostingL l))) l ()
  , sorter :: Seq (Filtered (TransactionL l, View (Converted (PostingL l)))
      -> l (Seq (Filtered (TransactionL l, View (Converted (PostingL l))))))
  , postFilter :: Matcher (RunningBalance
      (Sorted (Filtered (TransactionL l, View (Converted (PostingL l)))))) l ()
  , outstream :: Last (Consumer ByteString IO ())
  }

data ClatchDefaults = ClatchDefaults

clatcher :: ClatchOptions l -> IO ()
clatcher = undefined
