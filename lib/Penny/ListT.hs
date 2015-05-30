{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Penny.ListT where

import Control.Applicative
import Control.Monad
import qualified Pipes as P
import Penny.Ledger
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | For the time being this is just a wrapper on 'P.ListT' from
-- pipes.  At some point it might change to something based on the
-- operational package, to allow for improved logging.
newtype ListT m a = ListT (P.ListT m a)
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, Monoid)

makeWrapped ''ListT

instance MonadIO m => MonadIO (ListT m) where
  liftIO = ListT . liftIO

instance Ledger m => Ledger (ListT m) where
  type PriceL (ListT m) = PriceL m
  type TransactionL (ListT m) = TransactionL m
  type TreeL (ListT m) = TreeL m
  type PostingL (ListT m) = PostingL m

  vault = ListT $ lift Penny.Ledger.vault
  dateTime = ListT . lift . Penny.Ledger.dateTime
  fromTo = ListT . lift . Penny.Ledger.fromTo
  exchange = ListT . lift . Penny.Ledger.exchange
  scalar = ListT . lift . Penny.Ledger.scalar
  realm = ListT . lift . Penny.Ledger.realm
  offspring = ListT . lift . Penny.Ledger.offspring
  txnMeta = ListT . lift . Penny.Ledger.txnMeta
  topLineSer = ListT . lift . Penny.Ledger.topLineSer
  postings = ListT . lift . Penny.Ledger.postings
  pstgMeta = ListT . lift . Penny.Ledger.pstgMeta
  trio = ListT . lift . Penny.Ledger.trio
  qty = ListT . lift . Penny.Ledger.qty
  commodity = ListT . lift . Penny.Ledger.commodity
  postingSer = ListT . lift . Penny.Ledger.postingSer

observe :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
observe (ListT (P.Select pdcr)) = liftM eval . P.next $ pdcr
  where
    eval (Left ()) = Nothing
    eval (Right (a, pdcr')) = Just (a, ListT (P.Select pdcr'))

observeAll :: Monad m => ListT m a -> m (Seq a)
observeAll list = do
  mayR <- observe list
  case mayR of
    Nothing -> return Seq.empty
    Just (x, xs) -> do
      rest <- observeAll xs
      return $ x <| rest
