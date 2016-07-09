{-# LANGUAGE TemplateHaskell #-}
module Penny.Tree.Harvester where

import Penny.Scalar
import Penny.Tree

import qualified Control.Lens as Lens
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

-- | Run a pre-order search for the 'Tree' matching the given
-- predicate.  Stops searching if a matching 'Tree' is found; if no
-- result, return 'Nothing'.
treePreOrder
  :: (Tree -> Maybe a)
  -> Tree
  -> Maybe a
treePreOrder pd t = case pd t of
  Just r -> Just r
  Nothing -> forestPreOrder pd . _children $ t

-- | Runs a pre-order search in a forest for a 'Tree' matching the
-- given predicate.  Stops searching if a matching 'Tree' is found; if
-- no result, return 'Nothing'.
forestPreOrder
  :: (Tree -> Maybe a)
  -> Seq Tree
  -> Maybe a
forestPreOrder pd
  = listToMaybe
  . catMaybes
  . fmap (treePreOrder pd)
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

-- | If the tree has a scalar of 'SLabel' with the given text, returns
-- the children.
labeledTree :: Text -> Tree -> Maybe (Seq Tree)
labeledTree txt tree = do
  scalar <- _scalar tree
  lbl <- Lens.preview _SLabel scalar
  guard (lbl == txt)
  return . _children $ tree


-- | If the given 'Tree' has no children, returns the 'Scalar' of the
-- 'Tree'.  Otherwise, returns 'Nothing'.
childlessTree :: Tree -> Maybe Scalar
childlessTree tree = do
  guard (Seq.null . Lens.view children $ tree)
  Lens.view scalar tree

-- | Returns True if the given Tree has at least one child.
treeHasChild :: Tree -> Bool
treeHasChild tree
  = (not . Seq.null . _children $ tree)

data Grove = Grove
  { _topLineForest :: Seq Tree
  , _postingForest :: Seq Tree
  } deriving Show

Lens.makeLenses ''Grove

