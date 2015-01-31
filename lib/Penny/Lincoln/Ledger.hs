{-# LANGUAGE TypeFamilies #-}

module Penny.Lincoln.Ledger where

import Control.Monad
import Control.Applicative
import Data.Functor.Identity
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.Commodity
import Control.Monad.Trans.Class
import Penny.Lincoln.Transaction
import qualified Data.Foldable as Fdbl
import Penny.Lincoln.Ents
import Penny.Lincoln.Ent

class (Applicative l, Functor l, Monad l) => Ledger l where
  type TransactionL (l :: * -> *) :: *
  type TreeL (l :: * -> *) :: *
  type PostingL (l :: * -> *) :: *

  transactions :: l [TransactionL l]
  transactionMeta :: TransactionL l -> l [TreeL l]
  scalar :: TreeL l -> l Scalar
  children :: TreeL l -> l [TreeL l]
  postings :: TransactionL l -> l [PostingL l]
  postingTrees :: PostingL l -> l [TreeL l]
  postingTrio :: PostingL l -> l Trio
  postingQty :: PostingL l -> l Qty
  postingCommodity :: PostingL l -> l Commodity

data PlainT m a = PlainT ([Transaction] -> m a)

type Plain = PlainT Identity

instance Functor m => Functor (PlainT m) where
  fmap f (PlainT v) = PlainT $ \ts -> fmap f (v ts)

instance Monad m => Monad (PlainT m) where
  return a = PlainT $ \_ -> return a
  PlainT l >>= k = PlainT $ \ts -> do
    rl <- l ts
    let PlainT kk = k rl
    kk ts

instance (Monad m, Functor m) => Applicative (PlainT m) where
  pure = return
  (<*>) = ap

instance MonadTrans PlainT where
  lift = PlainT . return

data Posting = Posting [Tree] Trio Qty Commodity
  deriving (Eq, Ord, Show)

balancedToPostings :: Balanced PstgMeta -> [Posting]
balancedToPostings = Fdbl.toList . fmap f . balancedToSeqEnt
  where
    f (Ent q cy (PstgMeta ts tri)) = Posting ts tri q cy

instance (Applicative m, Monad m) => Ledger (PlainT m) where
  type TransactionL (PlainT m) = Transaction
  type TreeL (PlainT m) = Tree
  type PostingL (PlainT m) = Posting

  transactions = PlainT $ return
  transactionMeta (Transaction (TopLine ts) _) = PlainT . const . return $ ts
  scalar (Tree n _) = PlainT . const . return $ n
  children (Tree _ cs) = PlainT . const . return $ cs
  postings (Transaction _ bal) = PlainT . const
    . return . balancedToPostings $ bal
  postingTrees (Posting ts _ _ _) = PlainT . const . return $ ts
  postingTrio (Posting _ tr _ _) = PlainT . const . return $ tr
  postingQty (Posting _ _ q _) = PlainT . const . return $ q
  postingCommodity (Posting _ _ _ cy) = PlainT . const . return $ cy

newtype TxnId = TxnId Int
  deriving (Eq, Ord, Show)

newtype TreeId = TreeId Int
  deriving (Eq, Ord, Show)

newtype PostingId = PostingId Int
  deriving (Eq, Ord, Show)

data SqlState = SqlState

data Sql a = Sql (SqlState -> IO a)

instance Functor Sql where
  fmap f (Sql k) = Sql $ \st -> fmap f (k st)

instance Monad Sql where
  return a = Sql $ \_ -> return a
  (Sql l) >>= k = Sql $ \st -> do
    a <- l st
    let Sql kr = k a
    kr st

instance Applicative Sql where
  pure = return
  (<*>) = ap

instance Ledger Sql where
  type TransactionL Sql = TxnId
  type TreeL Sql = TreeId
  type PostingL Sql = PostingId

allQtys :: Ledger l => l [Qty]
allQtys = do
  txns <- transactions
  pstgs <- fmap concat . mapM postings $ txns
  mapM postingQty pstgs
