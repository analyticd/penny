{-# LANGUAGE GADTs #-}

module Penny.LedgerOp where

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

vault :: Ledger (Seq (Seq (Either PriceL TransactionL)))
vault = singleton Vault

data PriceL = PriceL
  { dateTime :: Ledger DateTime
  , fromTo :: Ledger FromTo
  , exchange :: Ledger Exch
  }

data TreeL = TreeL
  { scalar :: Ledger (Maybe Scalar)
  , realm :: Ledger Realm
  , offspring :: Ledger (Seq TreeL)
  }

data TransactionL = TransactionL
  { txnMeta :: Ledger (Seq TreeL)
  , topLineSer :: Ledger TopLineSer
  , postings :: Ledger (Seq PostingL)
  }

data PostingL = PostingL
  { pstgMeta :: Ledger (Seq TreeL)
  , trio :: Ledger Trio
  , qty :: Ledger Qty
  , commodity :: Ledger Commodity
  , postingSer :: Ledger PostingSer
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
      , Penny.LedgerOp.qty = return qt
      , Penny.LedgerOp.commodity = return cy
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
    _ -> fmap S3c . Penny.LedgerOp.qty $ p
