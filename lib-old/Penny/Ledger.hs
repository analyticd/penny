{-# LANGUAGE GADTs #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Ledger where
{-

import Data.Sums
import Penny.Amount
import Penny.Ent
import Penny.Ents
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
import Control.Monad.Operational
import qualified Data.Sequence as Seq

data LedgerI a where
  Vault :: LedgerI (Seq (Seq (Either PriceL TransactionL)))

type Ledger = Program LedgerI

-- | All the items contained in the Ledger.
vault :: Ledger (Seq (Seq (Either PriceL TransactionL)))
vault = singleton Vault

data PriceL = PriceL
  { dateTime :: Ledger DateTime
  -- ^ When this price became effective
  , fromTo :: Ledger FromTo
  -- ^ 1 unit of the from commodity equals the given number of
  -- exchange commodity
  , exchange :: Ledger Exch
  -- ^ 1 unit of the from commodity in the 'trade' equals this much of
  -- the to commodity in the 'trade'
  }

data TreeL = TreeL
  { scalar :: Ledger (Maybe Scalar)
  -- ^ Information held in this node of the tree.
  , realm :: Ledger Realm
  -- ^ Each tree is in a particular realm.
  , offspring :: Ledger (Seq TreeL)
  -- ^ Child trees of a particular tree.
  }

data TransactionL = TransactionL
  { txnMeta :: Ledger (Seq TreeL)
  -- ^ The metadata for the transaction.
  , topLineSer :: Ledger TopLineSer
  -- ^ The serial that applies to the entire transction.
  , postings :: Ledger (Seq PostingL)
  -- ^ All postings.  A posting that is in a transaction is a @plink@.
  }

data PostingL = PostingL
  { pstgMeta :: Ledger (Seq TreeL)
  -- ^ Metadata for a single plink.
  , trio :: Ledger Trio
  -- ^ The Trio that belongs to a posting.
  , qty :: Ledger Qty
  -- ^ The quantity that belongs to a posting.
  , commodity :: Ledger Commodity
  -- ^ The unit of currency for the posting.
  , postingSer :: Ledger PostingSer
  -- ^ The serial that belongs to a posting.
  }

scroll
  :: Seq (Seq (Either Price (Transaction TopLineSer PostingSer)))
  -> Ledger a
  -> a
scroll sq led = case view led of
  Return a -> a
  Vault :>>= k -> scroll sq (k (convertScroll sq))

convertScroll
  :: Seq (Seq (Either Price (Transaction TopLineSer PostingSer)))
  -> Seq (Seq (Either PriceL TransactionL))
convertScroll = fmap (fmap convert)
  where
    convert (Left (Price dt tr ex))
      = Left (PriceL (return dt) (return tr) (return ex))
    convert (Right (Transaction (TopLine ts zk) bal))
      = Right (TransactionL tm (return zk) ps)
      where
        tm = return . Seq.fromList . map convertTree $ ts
        ps = return . convertPostings $ bal

convertTree :: Tree -> TreeL
convertTree (Tree rlm mayScalar children) = TreeL
  (return mayScalar) (return rlm)
  (return . Seq.fromList . map convertTree $ children)

convertPostings
  :: Balanced (PstgMeta PostingSer)
  -> Seq PostingL
convertPostings = fmap mkPosting . balancedToSeqEnt
  where
    mkPosting (Ent (Amount cy qt) (PstgMeta trees tri pstgSer)) = PostingL
      { pstgMeta = return . Seq.fromList . map convertTree $ trees
      , trio = return tri
      , Penny.Ledger.qty = return qt
      , Penny.Ledger.commodity = return cy
      , postingSer = return pstgSer
      }


-- | Gets the 'Qty' using the 'Trio' in the 'PostingL'.
originalQtyRep
  :: PostingL
  -> Ledger (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
originalQtyRep p = do
  tri <- trio p
  case tri of
    QC qr _ _ -> return $ S3b qr
    Q qr -> return $ S3b qr
    UC rn _ _ -> return $ S3a rn
    U rn -> return $ S3a rn
    _ -> fmap S3c . Penny.Ledger.qty $ p
-}
