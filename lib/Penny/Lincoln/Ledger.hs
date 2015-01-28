{-# LANGUAGE MultiParamTypeClasses #-}
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

class Ledger l txn tree posting where
  transactions :: Monad m => l m [txn]
  transactionMeta :: Monad m => txn -> l m [tree]
  scalar :: Monad m => tree -> l m Scalar
  children :: Monad m => tree -> l m [tree]
  postings :: Monad m => txn -> l m [posting]
  postingTrees :: Monad m => posting -> l m [tree]
  postingTrio :: Monad m => posting -> l m Trio
  postingQty :: Monad m => posting -> l m Qty
  postingCommodity :: Monad m => posting -> l m Commodity

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

instance Ledger Plain Transaction Tree Posting where
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

data Sql m a = Sql (SqlState -> IO (m a))

