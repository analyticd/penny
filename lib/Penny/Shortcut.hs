{-# LANGUAGE OverloadedStrings #-}
module Penny.Shortcut where

import Penny.Clatch
import Penny.Realm
import Penny.Scalar
import Penny.Tree

import Control.Applicative ((<|>))
import Control.Lens
  ( view, preview, to, uncons, (<|), unsnoc,
    _head, Getter )
import Control.Monad (guard)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay)

-- | Looks for a payee by looking in the top line
-- for the first tree whose root node is text, that has no children,
-- and whose name is not surrounded by parentheses.  Does not look in
-- postings.  If no payee is found, an empty string is returned.
payee :: Getter (Transaction, a) Text
payee = to $ fromMaybe "" . searchTopForest pd . view (transaction . trees)
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

account :: Getter (Sliced a) (Seq Text)
account = to $ fromMaybe Seq.empty . searchTopForest pd . view (posting . trees)
  where
    pd tree = do
      guard (view realm tree == User)
      guard (view (scalar . to isNothing) tree)
      accts <- sequence . fmap childlessUserTree . view children $ tree
      sequence . fmap (preview _SText) $ accts

-- | Creates a 'Tree' that will be recognized as an account.

accountTree
  :: Text
  -- ^ First sub-account
  -> Seq Text
  -- ^ Remaining sub-accounts
  -> Tree
accountTree c1 sq = Tree User Nothing (mkChild c1 <| fmap mkChild sq)
  where
    mkChild txt = Tree User (Just (SText txt)) Seq.empty

-- | Looks for tags by looking in the top line for the first tree
-- whose root node is empty and that has a non-empty list of children.
-- Does not look in postings.
tags :: Getter (Transaction, a) (Seq Text)
tags
  = to
  $ fromMaybe Seq.empty . searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      guard $ userTreeHasChild tree
      guard (view (scalar . to isNothing) tree)
      tgs <- sequence . fmap childlessUserTree . view children $ tree
      sequence . fmap (preview _SText) $ tgs

-- | Looks for a flag by looking for the first tree whose root node is
-- text, is surrounded by parentheses, is not empty, and has no
-- children.  Looks in posting first, then in top line.  Only the main
-- characters (not the parentheses) are returned.  If there is no text
-- inside the parentheses, an empty string is returned.  If there is
-- no flag at all, an empty string is returned.
flag :: Getter (Sliced a) Text
flag = to f
  where
    f slice = fromMaybe "" $ search posting <|> search transaction
      where
        search lens = searchTopForest pd . view (lens . trees) $ slice
          where
            pd tree = do
              sc <- childlessUserTree tree
              name <- preview _SText sc
              (firstChar, restChars) <- uncons name
              guard (firstChar == '(')
              (mainChars, lastChar) <- unsnoc restChars
              guard (lastChar == ')')
              return mainChars

-- | Looks for a 'flag' with a value of @R@.

reconciled :: Getter (Sliced a) Bool
reconciled = flag . to (== "R")

-- | Searches for the first tree whose root node is an integer and
-- that has no children.  If no such node is found in the posting,
-- search the top line.
number :: Getter (Sliced a) (Maybe Integer)
number = to f
  where
    f slice = search posting <|> search transaction
      where
        search lens = searchTopForest pd . view (lens . trees) $ slice
          where
            pd tree = do
              sc <- childlessUserTree tree
              preview _SInteger sc

-- | Searches the top line for the first tree whose root node is a
-- date and that has no children.  Does not search the postings.
date :: Getter (Transaction, a) (Maybe Day)
date = to $ searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      sc <- childlessUserTree tree
      preview _SDay sc

-- | Searches the top line for the first tree whose root node is a
-- time and that has no children.  Does not search the postings.
time :: Getter (Transaction, a) (Maybe TimeOfDay)
time = to $ searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      sc <- childlessUserTree tree
      preview _STime sc

-- | Searches the top line for the first tree whose root node is a
-- zone and that has no children.  Does not search the postings.
zone :: Getter (Transaction, a) (Maybe Int)
zone = to $ searchTopForest pd . view (transaction . trees)
  where
    pd tree = do
      sc <- childlessUserTree tree
      preview _SZone sc
