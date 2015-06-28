{-# LANGUAGE OverloadedStrings #-}

module Penny.Ledger.Matcher where

{-

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Ledger (Ledger, TreeL)
import qualified Penny.Ledger
import Penny.Matcher
import Penny.Field.Matcher
import Control.Monad.Trans.Class

-- | Succeds only if this 'TreeL' has offspring.
hasOffspring
  :: (Monad (t Ledger), Alternative (t Ledger), MonadTrans t)
  => TreeL
  -> t Ledger ()
hasOffspring
  = guard . not . Seq.null
  <=< lift . Penny.Ledger.offspring

-- # Trees

-- | Conduct a pre-order search for a tree with the given name.
-- Returns all child trees.
namedTree
  :: (Monad (t Ledger), Alternative (t Ledger), MonadTrans t)
  => Text
  -> Seq TreeL
  -> t Ledger (Seq TreeL)
namedTree = undefined
{-
namedTree nm = each . preOrder $ \tr -> do
  txt <- text <=< just <=< lift Penny.Ledger.scalar $ tr
  guard $ txt == nm
  lift $ Penny.Ledger.offspring tr
-}

{-
  :: (Ledger m, MonadPlus m)
  => Text
  -> Seq (TreeL m)
  -> m (Seq (TreeL m))
namedTree nm = each . preOrder $ \tr -> do
  txt <- text <=< just <=< Penny.Ledger.scalar $ tr
  guard $ txt == nm
  Penny.Ledger.offspring tr
-}

-- | Traverses this tree and all child trees, in pre-order; that is,
-- this node is visited, followed by all child nodes.
preOrder
  :: (Monad (t Ledger), Alternative (t Ledger), MonadTrans t)
  => (TreeL -> t Ledger a)
  -> TreeL
  -> t Ledger a
preOrder mtcr s = do
  cs <- lift $ Penny.Ledger.offspring s
  (mtcr s) <|> (F.asum . fmap (preOrder mtcr $) $ cs)

-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: (Monad (t Ledger), Alternative (t Ledger), MonadTrans t)
  => (TreeL -> t Ledger a)
  -> TreeL
  -> t Ledger a
postOrder mtcr s = do
  cs <- lift $ Penny.Ledger.offspring s
  (F.asum . fmap (preOrder mtcr $) $ cs) <|> (mtcr s)

-}
