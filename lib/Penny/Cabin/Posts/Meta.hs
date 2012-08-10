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
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Cabin.Meta as M
import qualified Penny.Cabin.Options as CO

-- | The Box type that is used throughout the Posts modules.
type Box = L.Box PostMeta

data PostMeta =
  PostMeta { filteredNum :: Ly.FilteredNum
            , sortedNum :: Ly.SortedNum
            , visibleNum :: M.VisibleNum
            , balance :: Maybe L.Balance }
  deriving Show


addMetadata ::
  [(L.Box (Ly.LibertyMeta, Maybe L.Balance))]
  -> [Box]
addMetadata = M.visibleNums f where
  f vn (lm, mb) =
    PostMeta (Ly.filteredNum lm) (Ly.sortedNum lm) vn mb

-- | Adds appropriate metadata, including the running balance, to a
-- list of Box. Because all posts are incorporated into the running
-- balance, first calculates the running balance for all posts. Then,
-- removes posts we're not interested in by applying the predicate and
-- the post-filter. Finally, adds on the metadata, which will include
-- the VisibleNum.
toBoxList ::
  CO.ShowZeroBalances
  -> (L.Box Ly.LibertyMeta -> Bool)
  -- ^ Removes posts from the report if applying this function to the
  -- post returns False. Posts removed still affect the running
  -- balance.
  
  -> [Ly.PostFilterFn]
  -- ^ Applies these post-filters to the list of posts that results
  -- from applying the predicate above. Might remove more
  -- postings. Postings removed still affect the running balance.

  -> [L.Box Ly.LibertyMeta]
  -> [Box]
toBoxList szb pdct pff =
  addMetadata
  . Ly.processPostFilters pff
  . filter (pdct . fmap fst)
  . addBalances szb

addBalances ::
  CO.ShowZeroBalances
  -> [L.Box Ly.LibertyMeta]
  -> [(L.Box (Ly.LibertyMeta, Maybe L.Balance))]
addBalances szb = snd . mapAccumL (balanceAccum szb) Nothing

balanceAccum :: 
  CO.ShowZeroBalances
  -> Maybe L.Balance
  -> L.Box Ly.LibertyMeta
  -> (Maybe L.Balance, (L.Box (Ly.LibertyMeta, Maybe L.Balance)))
balanceAccum (CO.ShowZeroBalances szb) mb po =
  let balThis = L.entryToBalance . Q.entry . L.boxPostFam $ po
      balNew = case mb of
        Nothing -> balThis
        Just balOld -> L.addBalances balOld balThis
      balNoZeroes = L.removeZeroCommodities balNew
      bal' = if szb then Just balNew else balNoZeroes
      po' = L.Box (L.boxMeta po, bal') (L.boxPostFam po)
  in (bal', po')

