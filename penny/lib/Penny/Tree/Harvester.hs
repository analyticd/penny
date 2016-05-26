{-# LANGUAGE TemplateHaskell #-}
module Penny.Tree.Harvester where

import Penny.Realm
import Penny.Scalar
import Penny.Tree

import qualified Control.Lens as Lens
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

-- | If the given 'Tree' has no children that are in the 'User' realm,
-- returns the 'Scalar' of the 'Tree'.  Otherwise, returns 'Nothing'.
childlessUserTree :: Tree -> Maybe Scalar
childlessUserTree tree = do
  guard (Lens.view realm tree == User)
  guard (Seq.null . Seq.filter ((== User) . Lens.view realm)
    . Lens.view children $ tree)
  Lens.view scalar tree

-- | Returns True if the given Tree is a User tree and if it has at
-- least one child that is a User tree.
userTreeHasChild :: Tree -> Bool
userTreeHasChild tree
  = Lens.view realm tree == User
  && (not . Seq.null . Seq.filter ((== User) . Lens.view realm) . _children $ tree)

data Grove = Grove
  { _topLineForest :: Seq Tree
  , _postingForest :: Seq Tree
  } deriving Show

Lens.makeLenses ''Grove

