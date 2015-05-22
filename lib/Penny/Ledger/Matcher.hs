{-# LANGUAGE OverloadedStrings #-}

module Penny.Ledger.Matcher where

import Control.Applicative
import qualified Data.Foldable as F
import Control.Monad.Trans.Class
import qualified Data.Sequence as Seq
import Penny.Ledger (Ledger, TreeL)
import qualified Penny.Ledger
import Penny.Matcher

-- | Succeds only of this 'TreeL' has offspring.
hasOffspring :: Ledger m => Matcher (TreeL m) m ()
hasOffspring = do
  tr <- getSubject
  sq <- lift $ Penny.Ledger.offspring tr
  if Seq.null sq then return () else empty

-- # Trees

-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: Ledger m
  => Matcher (TreeL m) m a
  -> Matcher (TreeL m) m a
preOrder mtcr = do
  s <- getSubject
  cs <- lift $ Penny.Ledger.offspring s
  mtcr <|> (F.asum . fmap (study (preOrder mtcr)) $ cs)


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: Ledger m
  => Matcher (TreeL m) m a
  -> Matcher (TreeL m) m a
postOrder mtcr = do
  s <- getSubject
  cs <- lift $ Penny.Ledger.offspring s
  (F.asum . fmap (study (postOrder mtcr)) $ cs) <|> mtcr

