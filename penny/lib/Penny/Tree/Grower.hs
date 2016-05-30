{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Penny.Tree.Grower where

import Penny.Tree
import Penny.Scalar

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X

-- | Creates a single user tree with no children.
bachelor :: Scalar -> Tree
bachelor s = Tree (Just s) Seq.empty

-- | Creates a User tree with the given children.
family :: Scalar -> Seq Tree -> Tree
family s cs = Tree (Just s) cs

-- | Creates a User tree with no 'Scalar' and the given children.
orphans :: Seq Tree -> Tree
orphans = Tree Nothing

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

