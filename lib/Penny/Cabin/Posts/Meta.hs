module Penny.Cabin.Posts.Meta (
  M.VisibleNum(M.unVisibleNum)
  , PostMeta(filteredNum, sortedNum, visibleNum, balance)
  , Box
  , toBoxList
  ) where

import Data.List (mapAccumL)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty as Ly
import qualified Penny.Cabin.Meta as M
import qualified Penny.Cabin.Options as CO
import qualified Penny.Steel.PredTree as Pe
import Data.Monoid (mempty, mappend)

-- | The Box type that is used throughout the Posts modules.
type Box = L.Box PostMeta

data PostMeta =
  PostMeta { filteredNum :: Ly.FilteredNum
            , sortedNum :: Ly.SortedNum
            , visibleNum :: M.VisibleNum
            , balance :: L.Balance }
  deriving Show


addMetadata ::
  [(L.Box (Ly.LibertyMeta, L.Balance))]
  -> [Box]
addMetadata = M.visibleNumBoxes f where
  f vn (lm, b) =
    PostMeta (Ly.filteredNum lm) (Ly.sortedNum lm) vn b

-- | Adds appropriate metadata, including the running balance, to a
-- list of Box. Because all posts are incorporated into the running
-- balance, first calculates the running balance for all posts. Then,
-- removes posts we're not interested in by applying the predicate and
-- the post-filter. Finally, adds on the metadata, which will include
-- the VisibleNum.
toBoxList ::
  CO.ShowZeroBalances
  -> Pe.Pdct (L.Box Ly.LibertyMeta)
  -- ^ Removes posts from the report if applying this function to the
  -- post returns a value other than Just True. Posts removed still
  -- affect the running balance.

  -> [Ly.PostFilterFn]
  -- ^ Applies these post-filters to the list of posts that results
  -- from applying the predicate above. Might remove more
  -- postings. Postings removed still affect the running balance.

  -> [L.Box Ly.LibertyMeta]
  -> [Box]
toBoxList szb pdct pff =
  addMetadata
  . Ly.processPostFilters pff
  . filter (maybe False id . Pe.eval pdct . fmap fst)
  . addBalances szb

addBalances ::
  CO.ShowZeroBalances
  -> [L.Box Ly.LibertyMeta]
  -> [(L.Box (Ly.LibertyMeta, L.Balance))]
addBalances szb = snd . mapAccumL (balanceAccum szb) mempty

balanceAccum :: 
  CO.ShowZeroBalances
  -> L.Balance
  -> L.Box Ly.LibertyMeta
  -> (L.Balance, (L.Box (Ly.LibertyMeta, L.Balance)))
balanceAccum (CO.ShowZeroBalances szb) balOld po =
  let balThis = L.entryToBalance . Q.entry . L.boxPostFam $ po
      balNew = mappend balOld balThis
      balNoZeroes = L.removeZeroCommodities balNew
      bal' = if szb then balNew else balNoZeroes
      po' = L.Box (L.boxMeta po, bal') (L.boxPostFam po)
  in (bal', po')

