-- | Grab bag of utility functions.

module Penny.Cabin.Balance.Util where

import qualified Penny.Lincoln.NestedMap as NM
import qualified Data.Foldable as Fdbl
import qualified Data.List.NonEmpty as NE
import qualified Data.Tree as T


-- | Takes a list of postings and puts them into a Forest. Each level
-- of each of the trees corresponds to a sub account. The label of the
-- node tells you the sub account name and gives you a list of the
-- postings at that level.
tieredForest ::
  Ord k
  => (a -> Fdbl.Foldable k)
  -- ^ Extracts a key from the elements we are putting in the tree. 
  -> Fdbl.Foldable a
  -> T.Forest (k, [a])
tieredForest getKeys ls
