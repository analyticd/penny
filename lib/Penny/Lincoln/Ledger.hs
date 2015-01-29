{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Penny.Lincoln.Ledger where

import Control.Monad
import Control.Applicative
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.Commodity
import Control.Monad.Trans.Class
import Penny.Lincoln.Transaction
import qualified Data.Foldable as Fdbl
import Penny.Lincoln.Ents
import Penny.Lincoln.Ent

class Ledger l txn tree posting | l -> txn, l -> tree, l -> posting where
  transactions :: l [txn]
  transactionMeta :: txn -> l [tree]
  scalar :: tree -> l Scalar
  children :: tree -> l [tree]
  postings :: txn -> l [posting]
  postingTrees :: posting -> l [tree]
  postingTrio :: posting -> l Trio
  postingQty :: posting -> l Qty
  postingCommodity :: posting -> l Commodity

data Plain m a = Plain ([Transaction] -> m a)

instance Functor m => Functor (Plain m) where
  fmap f (Plain v) = Plain $ \ts -> fmap f (v ts)

instance Monad m => Monad (Plain m) where
  return a = Plain $ \_ -> return a
  Plain l >>= k = Plain $ \ts -> do
    rl <- l ts
    let Plain kk = k rl
    kk ts

instance (Monad m, Functor m) => Applicative (Plain m) where
  pure = return
  (<*>) = ap

instance MonadTrans Plain where
  lift = Plain . return

data Posting = Posting [Tree] Trio Qty Commodity
  deriving (Eq, Ord, Show)

balancedToPostings :: Balanced PstgMeta -> [Posting]
balancedToPostings = Fdbl.toList . fmap f . balancedToSeqEnt
  where
    f (Ent q cy (PstgMeta ts tri)) = Posting ts tri q cy

instance Monad m => Ledger (Plain m) Transaction Tree Posting where
  transactions = Plain $ return
  transactionMeta (Transaction (TopLine ts) _) = Plain . const . return $ ts
  scalar (Tree n _) = Plain . const . return $ n
  children (Tree _ cs) = Plain . const . return $ cs
  postings (Transaction _ bal) = Plain . const
    . return . balancedToPostings $ bal
  postingTrees (Posting ts _ _ _) = Plain . const . return $ ts
  postingTrio (Posting _ tr _ _) = Plain . const . return $ tr
  postingQty (Posting _ _ q _) = Plain . const . return $ q
  postingCommodity (Posting _ _ _ cy) = Plain . const . return $ cy

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

instance Ledger Sql TxnId TreeId PostingId where

filterTxns
  :: Ledger l tx tr ps
  => [tx]
  -> l [tx]
filterTxns = undefined
