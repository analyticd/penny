{-# LANGUAGE OverloadedStrings #-}
module Penny.Shortcut where

import Control.Applicative
import Control.Lens hiding (children)
import Control.Monad (guard)
import Penny.Clatch
import Penny.Tree
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Scalar
import Data.Maybe
import Penny.Realm
import Data.Text (Text)
import Data.Time

-- | Looks for a payee by looking in the top line
-- for the first tree whose root node is text, that has no children,
-- and whose name is not surrounded by parentheses.  Does not look in
-- postings.
payee :: (Transaction, a) -> Maybe Text
payee = searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      sc <- childlessUserTree tree
      name <- preview _SText sc
      firstChar <- preview _head name
      guard (firstChar /= '(')
      return name

-- | Looks for an account by searching the postings for the first tree
-- whose root node is empty and that has a non-empty list of children,
-- each of which has no children.  Does not look in the top line.

account :: Sliced a -> Maybe (Seq Scalar)
account = searchTopForest pd . view (posting . trees)
  where
    pd tree = do
      guard (view realm tree == User)
      guard (view (scalar . to isNothing) tree)
      guard (view (children . to (not . Seq.null)) tree)
      sequence . fmap childlessUserTree . view children $ tree

-- | Looks for tags by looking in the top line for the first tree
-- whose root node is empty and that has a non-empty list of children.
-- Does not look in postings.
tags :: (Transaction, a) -> Maybe (Seq Tree)
tags = searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      guard (view realm tree == User)
      guard (view (scalar . to isNothing) tree)
      guard (view (children . to (not . Seq.null)) tree)
      return (view children tree)

-- | Looks for a flag by looking for the first tree whose root node is
-- text, is surrounded by parentheses, and has no children.  Looks in
-- posting first, then in top line.
flag :: Sliced a -> Maybe Text
flag slice = search posting <|> search transaction
  where
    search lens = searchTopForest pd . view (lens . trees) $ slice
      where
        pd tree = do
          guard (view realm tree == User)
          guard (view (children . to (not . Seq.null)) tree)
          sc <- view scalar tree
          name <- preview _SText sc
          (firstChar, restChars) <- uncons name
          guard (firstChar == '(')
          (mainChars, lastChar) <- unsnoc restChars
          guard (lastChar == ')')
          return mainChars

-- | Searches for the first tree whose root node is an integer and
-- that has no children.  If no such node is found in the posting,
-- search the top line.
number :: Sliced a -> Maybe Integer
number slice = search posting <|> search transaction
  where
    search lens = searchTopForest pd . view (lens . trees) $ slice
      where
        pd tree = do
          guard (view realm tree == User)
          guard (view (children . to (not . Seq.null)) tree)
          sc <- view scalar tree
          preview _SInteger sc

-- | Searches the top line for the first tree whose root node is a
-- date and that has no children.  Does not search the postings.
date :: (Transaction, a) -> Maybe Day
date = searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      guard $ view realm tree == User
      guard $ view (children . to (not . Seq.null)) tree
      sc <- view scalar tree
      preview _SDay sc

-- | Searches the top line for the first tree whose root node is a
-- time and that has no children.  Does not search the postings.
time :: (Transaction, a) -> Maybe TimeOfDay
time = searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      guard $ view realm tree == User
      guard $ view (children . to (not . Seq.null)) tree
      sc <- view scalar tree
      preview _STime sc

-- | Searches the top line for the first tree whose root node is a
-- zone and that has no children.  Does not search the postings.
zone :: (Transaction, a) -> Maybe Int
zone = searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      guard $ view realm tree == User
      guard $ view (children . to (not . Seq.null)) tree
      sc <- view scalar tree
      preview _SZone sc
