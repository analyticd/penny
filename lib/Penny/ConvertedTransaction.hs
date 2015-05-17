{-# LANGUAGE TemplateHaskell #-}
module Penny.ConvertedTransaction where

import Control.Lens
import Control.Monad
import Penny.Converted
import Penny.Ledger
import qualified Penny.Ledger as Ledger
import Penny.SeqUtil
import Penny.ConvertedPosting
import Data.Sequence (Seq)
import qualified Data.Traversable as T

data ConvertedTransaction l = ConvertedTransaction
  { _transaction :: TransactionL l
  , _postings :: Seq (View (ConvertedPosting l))
  }

makeLenses ''ConvertedTransaction

convertTransaction
  :: Ledger l
  => Converter
  -> TransactionL l
  -> l (ConvertedTransaction l)
convertTransaction conv txn
  = return . ConvertedTransaction txn . allViews
  <=< T.mapM (convertPosting conv)
  <=< Ledger.postings
  $ txn
