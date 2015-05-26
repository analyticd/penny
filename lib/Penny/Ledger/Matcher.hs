{-# LANGUAGE OverloadedStrings #-}

module Penny.Ledger.Matcher where

import Control.Monad
import Data.Text (Text)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Ledger (Ledger, TreeL)
import qualified Penny.Ledger
import Penny.Matcher
import Penny.Field.Matcher

-- | Succeds only if this 'TreeL' has offspring.
hasOffspring :: (Ledger m, MonadPlus m) => TreeL m -> m ()
hasOffspring = false <=< return . Seq.null <=< Penny.Ledger.offspring

-- # Trees

-- | Conduct a pre-order search for a tree with the given name.
-- Returns all child trees.
namedTree
  :: (Ledger m, MonadPlus m)
  => Text
  -> Seq (TreeL m)
  -> m (Seq (TreeL m))
namedTree nm = each . preOrder $ \tr -> do
  txt <- text <=< just <=< Penny.Ledger.scalar $ tr
  guard $ txt == nm
  Penny.Ledger.offspring tr


-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: (Ledger m, MonadPlus m)
  => (TreeL m -> m a)
  -> TreeL m
  -> m a
preOrder mtcr s = do
  cs <- Penny.Ledger.offspring s
  mplus (mtcr s) (F.msum . fmap (preOrder mtcr $) $ cs)

-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: (Ledger m, MonadPlus m)
  => (TreeL m -> m a)
  -> TreeL m
  -> m a
postOrder mtcr s = do
  cs <- Penny.Ledger.offspring s
  mplus (F.msum . fmap (postOrder mtcr $) $ cs) (mtcr s)
