{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Penny.Tree where

import Control.Lens hiding (children)
import qualified Control.Lens as Lens
import Penny.Realm
import Penny.Scalar
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X

data Tree = Tree
  { _realm :: Realm
  , _scalar :: Maybe Scalar
  , _children :: Seq Tree
  } deriving (Eq, Ord, Show)

makeLenses ''Tree

-- | Run a pre-order search for the 'Tree' matching the given
-- predicate.  Stops searching if a matching 'Tree' is found; if no
-- result, return 'Nothing'.
findTreePreOrder
  :: (Tree -> Maybe a)
  -> Tree
  -> Maybe a
findTreePreOrder pd t = case pd t of
  Just r -> Just r
  Nothing -> searchForestPreOrder pd . _children $ t

-- | Runs a pre-order search in a forest for a 'Tree' matching the
-- given predicate.  Stops searching if a matching 'Tree' is found; if
-- no result, return 'Nothing'.
searchForestPreOrder
  :: (Tree -> Maybe a)
  -> Seq Tree
  -> Maybe a
searchForestPreOrder pd
  = listToMaybe
  . catMaybes
  . fmap (findTreePreOrder pd)
  . toList

-- | Search this top forest only.  Do not descend into child forests.
searchTopForest
  :: (Tree -> Maybe a)
  -> Seq Tree
  -> Maybe a
searchTopForest pd
  = listToMaybe
  . catMaybes
  . fmap pd
  . toList

-- | If the given 'Tree' has no children that are in the 'User' realm,
-- returns the 'Scalar' of the 'Tree'.  Otherwise, returns 'Nothing'.
childlessUserTree :: Tree -> Maybe Scalar
childlessUserTree tree = do
  guard (view realm tree == User)
  guard (Seq.null . Seq.filter ((== User) . view realm)
    . view children $ tree)
  view scalar tree

-- | Returns True if the given Tree is a User tree and if it has at
-- least one child that is a User tree.
userTreeHasChild :: Tree -> Bool
userTreeHasChild tree
  = view realm tree == User
  && (not . Seq.null . Seq.filter ((== User) . view realm) . _children $ tree)

-- | Creates a single user tree with no children.
bachelor :: Scalar -> Tree
bachelor s = Tree User (Just s) Seq.empty

-- | Creates a User tree with the given children.
family :: Scalar -> Seq Tree -> Tree
family s cs = Tree User (Just s) cs

-- | Creates a User tree with no 'Scalar' and the given children.
orphans :: Seq Tree -> Tree
orphans = Tree User Nothing

-- | If the payee name is not empty and does not start with an open
-- parenthesis, creates a tree whose root node is the payee name.
-- Otherwise, creates a tree with @payee@ for the root label and one
-- child with the name of the payee as its label.
payee :: Text -> Tree
payee p
  | not (X.null p) && noOpenParen p = bachelor (SText p)
  | otherwise = family (SText "payee") [bachelor (SText p)]
  where
    noOpenParen x = case Lens.uncons x of
      Nothing -> True
      Just (c, _) -> c /= '('

-- | If there is at least one sub-account, creates an unlabeled tree
-- for an account.  Otherwise, creates a labeled tree with no
-- children.
account :: Seq Text -> Tree
account ts
  | Seq.null ts = bachelor (SText (X.pack "account"))
  | otherwise = orphans . fmap bachelor . fmap SText $ ts

-- | If there is at least one tag, creates orphans for the tags.  If
-- no tags, creates a bachelor whose scalar is @tags@.
tags :: Seq Scalar -> Tree
tags ss
  | Seq.null ss = bachelor (SText "tags")
  | otherwise = orphans . fmap bachelor $ ss

-- | If the given Text is not empty, does not begin with an open
-- paren, and does not end in a close paren, then wraps the Text in
-- parens and creates a bachelor.  Otherwise, creates an only child
-- whose parent's Scalar is @flag@.
flag :: Text -> Tree
flag x
  | not (X.null x) && noOpen x && noClose x = bachelor (SText x')
  | otherwise = family (SText "flag") [ bachelor (SText x) ]
  where
    x' = ('(' `Lens.cons` x) `Lens.snoc` ')'
    noOpen = maybe True (not . (== '(') . fst) . Lens.uncons
    noClose = maybe True (not . (== ')') . snd) . Lens.unsnoc

-- | Creates a bachelor with the given number.
number :: Integer -> Tree
number = bachelor . SInteger

-- TODO change shortcuts - do not use those for lookup - must also
-- lookup non-shortcuts
