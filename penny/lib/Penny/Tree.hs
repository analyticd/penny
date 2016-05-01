{-# LANGUAGE TemplateHaskell #-}
module Penny.Tree where

import Control.Lens hiding (children)
import Penny.Realm
import Penny.Scalar
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad
import qualified Data.Sequence as Seq

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
