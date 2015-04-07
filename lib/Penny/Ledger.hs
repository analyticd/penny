{-# LANGUAGE TypeFamilies #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Ledger where

import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Commodity
import Penny.Lincoln.Prices hiding (fromTo)
import Penny.Lincoln.Exch
import Data.Sequence (Seq)
import Penny.Lincoln.Transaction
import Control.Monad.Reader
import Penny.Matcher

class Monad l => Ledger l where
  type PriceL l
  type TransactionL l
  type TreeL l
  type PostingL l

  ------------------------------------------------
  -- All items
  ------------------------------------------------

  -- | All the items contained in the Ledger.
  vault :: l (Seq (Seq (Either (PriceL l) (TransactionL l))))

  ------------------------------------------------
  -- Prices
  ------------------------------------------------

  -- | When this price became effective
  dateTime :: PriceL l -> l DateTime

  -- | 1 unit of the from commodity equals the given number of
  -- exchange commodity
  fromTo :: PriceL l -> l FromTo

  -- | 1 unit of the from commodity in the 'trade' equals this much of
  -- the to commodity in the 'trade'
  exchange :: PriceL l -> l Exch

  ------------------------------------------------
  -- Trees
  ------------------------------------------------

  -- | Information held in this node of the tree.
  scalar :: TreeL l -> l (Maybe Scalar)

  -- | Each tree is in a particular realm.
  realm :: TreeL l -> l Realm

  -- | Child trees of a particular tree.
  offspring :: TreeL l -> l (Seq (TreeL l))


  ------------------------------------------------
  -- Transactions
  ------------------------------------------------

  -- | The metadata for the transaction.
  txnMeta :: TransactionL l -> l (Seq (TreeL l))

  -- | The serial that applies to the entire transction.
  topLineSer :: TransactionL l -> l TopLineSer

  -- | All postings.  A posting that is in a transaction is a @plink@.
  postings :: TransactionL l -> l (Seq (PostingL l))

  ------------------------------------------------
  -- Postings
  ------------------------------------------------
  -- | Metadata for a single plink.
  pstgMeta :: PostingL l -> l (Seq (TreeL l))

  -- | The Trio that belongs to a posting.
  trio :: PostingL l -> l Trio

  -- | The quantity that belongs to a posting.
  qty :: PostingL l -> l Qty

  -- | The unit of currency for the posting.
  commodity :: PostingL l -> l Commodity

  -- | The serial that belongs to a posting.
  postingSer :: PostingL l -> l PostingSer

instance Ledger m => Ledger (Matcher t m) where
  type PriceL (Matcher t m) = PriceL m
  type TransactionL (Matcher t m) = TransactionL m
  type TreeL (Matcher t m) = TreeL m
  type PostingL (Matcher t m) = PostingL m

  vault = lift vault
  dateTime = lift . dateTime
  fromTo = lift . fromTo
  exchange = lift . exchange
  scalar = lift . scalar
  realm = lift . realm
  offspring = lift . offspring
  txnMeta = lift . txnMeta
  topLineSer = lift . topLineSer
  postings = lift . postings
  pstgMeta = lift . pstgMeta
  trio = lift . trio
  qty = lift . qty
  commodity = lift . commodity
  postingSer = lift . postingSer
