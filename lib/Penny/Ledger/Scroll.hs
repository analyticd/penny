{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Scroll' is a an implementation of a 'Ledger'.  The 'Scroll' is
-- the simplest 'Ledger', as it simply takes a list of concrete prices
-- and transactions and stores them in memory.
module Penny.Ledger.Scroll where

import Penny.Amount
import Penny.Ledger
import Control.Applicative
import Control.Monad.Trans.Class
import Penny.Transaction
import Data.Functor.Identity
import Penny.Field
import Penny.Ents
import Penny.Ent
import Penny.Prices
import Control.Monad.Reader
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Environment
  = Seq (Seq (Either Price (Transaction TopLineSer PostingSer)))

newtype ScrollT m a
  = ScrollT (ReaderT Environment m a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadTrans
  , MonadReader Environment
  )

type Scroll = ScrollT Identity

instance (Applicative m, Monad m) => Ledger (ScrollT m) where
  newtype PriceL (ScrollT m) = PriceS Price
  newtype TransactionL (ScrollT m)
    = TransactionS (Transaction TopLineSer PostingSer)
  newtype TreeL (ScrollT m) = TreeS Tree
  newtype PostingL (ScrollT m) = PostingS (Ent (PstgMeta PostingSer))

  vault =
    asks (fmap (fmap (either (Left . PriceS) (Right . TransactionS))))
  dateTime (PriceS (Price dt _ _)) = return dt
  fromTo (PriceS (Price _ tr _)) = return tr
  exchange (PriceS (Price _ _ ex)) = return ex
  scalar (TreeS (Tree _ s _)) = return s
  realm (TreeS (Tree r _ _)) = return r
  offspring (TreeS (Tree _ _ ts))
    = return . fmap TreeS . Seq.fromList $ ts
  txnMeta (TransactionS (Transaction (TopLine ts _) _))
    = return . fmap TreeS . Seq.fromList $ ts
  topLineSer (TransactionS (Transaction (TopLine _ zk) _)) = return zk
  pstgMeta (PostingS (Ent _ (PstgMeta tr _ _)))
    = return . fmap TreeS . Seq.fromList $ tr
  postings (TransactionS (Transaction _ bal))
    = return . fmap PostingS . balancedToSeqEnt $ bal
  trio (PostingS (Ent _ (PstgMeta _ tri _))) = return tri
  qty (PostingS (Ent (Amount _ q) _)) = return q
  commodity (PostingS (Ent (Amount c _) _)) = return c
  postingSer (PostingS (Ent _ (PstgMeta _ _ a))) = return a
