{-# LANGUAGE OverloadedStrings #-}

module Penny.Clatch.Matcher where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Sequence (Seq)
import Data.Text (Text)
import Penny.Clatch
import Penny.Ledger
import Penny.Ledger.Matcher
import Penny.Matcher
import Penny.Field.Matcher
import Penny.Semantic.Matcher

-- | Creates a 'Matcher' that looks for a parent tree with the exact
-- name given.  First performs a pre-order search in the metadata of
-- the posting; then performs a pre-order search in the metadata for
-- the top line.  If successful, returns the child forest.
findNamedTree
  :: Ledger l
  => Text
  -> Matcher (Clatch l) l (Seq (TreeL l))
findNamedTree txt = matchPstg <|> matchTxn
  where
    finder = each . preOrder $ mtcr
    mtcr = do
      _ <- Penny.Ledger.Matcher.scalar . text . equal $ txt
      subj <- getSubject
      lift $ Penny.Ledger.offspring subj
    matchTxn = do
      txn <- fmap transactionL getSubject
      ts <- lift $ Penny.Ledger.txnMeta txn
      study finder ts
    matchPstg = do
      pstg <- fmap postingL getSubject
      ts <- lift $ Penny.Ledger.pstgMeta pstg
      study finder ts

payee :: Matcher (Clatch l) l Text
payee = undefined
