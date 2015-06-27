{-# LANGUAGE TypeFamilies #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Ledger where

import Data.Sums
import Penny.Field
import Penny.Trio
import Penny.Qty
import Penny.DateTime
import Penny.Commodity
import Penny.Price hiding (fromTo)
import Penny.Exch
import Data.Sequence (Seq)
import Penny.Transaction
import Penny.Representation
import Control.Monad

class Monad l => Ledger l where
  type PriceL l
  type TransactionL l
  type TreeL l
  type PostingL l

  ------------------------------------------------
  -- All items
  ------------------------------------------------

  vault :: l (Seq (Seq (Either (PriceL l) (TransactionL l))))

  ------------------------------------------------
  -- Prices
  ------------------------------------------------

  dateTime :: PriceL l -> l DateTime

  fromTo :: PriceL l -> l FromTo

  exchange :: PriceL l -> l Exch

  ------------------------------------------------
  -- Trees
  ------------------------------------------------

  scalar :: TreeL l -> l (Maybe Scalar)

  realm :: TreeL l -> l Realm

  offspring :: TreeL l -> l (Seq (TreeL l))


  ------------------------------------------------
  -- Transactions
  ------------------------------------------------

  txnMeta :: TransactionL l -> l (Seq (TreeL l))

  topLineSer :: TransactionL l -> l TopLineSer

  postings :: TransactionL l -> l (Seq (PostingL l))

  ------------------------------------------------
  -- Postings
  ------------------------------------------------
  pstgMeta :: PostingL l -> l (Seq (TreeL l))

  trio :: PostingL l -> l Trio

  qty :: PostingL l -> l Qty

  commodity :: PostingL l -> l Commodity

  postingSer :: PostingL l -> l PostingSer


-- | Gets the 'Qty' using the 'Trio' in the 'PostingL'.
originalQtyRep
  :: Ledger l
  => PostingL l
  -> l (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
originalQtyRep p = do
  tri <- trio p
  case tri of
    QC qr _ _ -> return $ S3b qr
    Q qr -> return $ S3b qr
    UC rn _ _ -> return $ S3a rn
    U rn -> return $ S3a rn
    _ -> liftM S3c . qty $ p
