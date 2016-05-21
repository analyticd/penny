{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Penny.Tree.HarvestLens where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import Data.Text (Text)

import Penny.Tree

data Grove = Grove
  { _topLineForest :: Lens.Lens' (Seq Tree) (Seq Tree)
  , _postingForest :: Lens.Lens' (Seq Tree) (Seq Tree)
  }

-- | Looks for a payee by looking at the top level of the postings
-- forest for a tree named @payee@ with one text child, then at the
-- top level of the top line forest for a tree named @payee@ with
-- one text child.  If none is found by that method, looks at the
-- top level of the top line forest for a tree with a text label
-- that has no children.  If none is found by that method, returns
-- Nothing.
payee
  :: Grove
  -> Maybe (Lens.Lens' Text Text)
payee = undefined
