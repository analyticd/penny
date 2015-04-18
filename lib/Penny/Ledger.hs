{-# LANGUAGE TypeFamilies #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Ledger where

import Penny.Field
import Penny.Trio
import Penny.Qty
import Penny.DateTime
import Penny.Commodity
import Penny.Price hiding (fromTo)
import Penny.Exch
import Data.Sequence (Seq)
import Penny.Transaction

class Monad l => Ledger l where
  data PriceL l
  data TransactionL l
  data TreeL l
  data PostingL l

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

