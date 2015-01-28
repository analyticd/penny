{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Lincoln.Ledger where

import Control.Applicative

newtype TxnId = TxnId Int
  deriving (Eq, Ord, Show)

newtype ForestId = ForestId Int
  deriving (Eq, Ord, Show)

newtype TreeId = TreeId Int
  deriving (Eq, Ord, Show)

newtype ScalarId = ScalarId Int
  deriving (Eq, Ord, Show)

newtype PostingId = PostingId Int
  deriving (Eq, Ord, Show)

class (Applicative l, Monad l) => Ledger l where
  transactions :: l [TxnId]
  transactionMeta :: TxnId -> l ForestId
  forestChildren :: ForestId -> l [TreeId]
  treeScalar :: TreeId -> l ScalarId
  postings :: TxnId -> l [PostingId]
