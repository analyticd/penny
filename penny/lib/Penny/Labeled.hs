{-# LANGUAGE OverloadedStrings #-}
module Penny.Labeled where

import Control.Applicative
import Control.Lens hiding (children)
import Control.Monad (guard)
import Penny.Clatch
import Penny.Tree
import Penny.Tree.Harvester
import Data.Sequence (Seq)
import Penny.Scalar
import Penny.Realm
import Data.Text (Text)
import Penny.SeqUtil

labeled :: Text -> Sliced a -> Maybe (Seq Tree)
labeled name slice = search posting <|> search transaction
  where
    search lens = searchTopForest pd (view (lens . trees) slice)
      where
        pd tree = do
          guard (view realm tree == User)
          sc <- view scalar tree
          subject <- preview _SText sc
          guard (name == subject)
          return . view children $ tree

payee :: Sliced a -> Maybe Text
payee sliced = do
  trees <- labeled "payee" sliced
  scalars <- sequence . fmap childlessUserTree $ trees
  scalar <- singleSeq scalars
  preview _SText scalar
