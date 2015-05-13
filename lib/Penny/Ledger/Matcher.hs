{-# LANGUAGE OverloadedStrings #-}

module Penny.Ledger.Matcher where

import Control.Applicative
import qualified Data.Foldable as F
import Control.Monad.Trans.Class
import qualified Data.Sequence as Seq
import Penny.Ledger
import Penny.Matcher
import Penny.Field

scalar :: Ledger m => Matcher (TreeL m) m Scalar
scalar = labelNest "scalar" Penny.Ledger.scalar just

-- | Succeds only of this 'TreeL' has offspring.
hasOffspring :: Ledger m => Matcher (TreeL m) m ()
hasOffspring = do
  tr <- getSubject
  sq <- lift $ Penny.Ledger.offspring tr
  if Seq.null sq
    then proclaim "tree has offspring" >> accept ()
    else proclaim "tree has no offspring" >> reject

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
  (inform "pre-order search - this node" >> indent mtcr) <|>
    (inform "pre-order search - children" >>
      (F.asum . fmap (indent . study (preOrder mtcr)) $ cs))


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: Ledger m
  => Matcher (TreeL m) m a
  -> Matcher (TreeL m) m a
postOrder mtcr = do
  s <- getSubject
  cs <- lift $ Penny.Ledger.offspring s
  (inform "post-order search - children" >>
    (F.asum . fmap (study (postOrder mtcr)) $ cs))
  <|> (inform "post-order search - this node" >> mtcr)

