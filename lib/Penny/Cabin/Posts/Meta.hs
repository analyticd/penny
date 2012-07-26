module Penny.Cabin.Posts.Meta (
  M.VisibleNum(M.unVisibleNum)
  , PostMeta(filteredNum, sortedNum, visibleNum, balance)
  , filterBoxes
  , addMetadata
  ) where

import Data.List (mapAccumL)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Cabin.Meta as M
import qualified Penny.Cabin.Options as CO

data PostMeta =
  PostMeta { filteredNum :: Ly.FilteredNum
            , sortedNum :: Ly.SortedNum
            , visibleNum :: M.VisibleNum
            , balance :: Maybe L.Balance }
  deriving Show


-- | Takes a list of Box with LibertyMeta, and a list of tokens from
-- the command line, and a list of post filters. Filters the list of
-- Box and processes the post filter. Fails if the expression fails to
-- produce a result.
filterBoxes ::
  [Exp.Token (L.Box Ly.LibertyMeta -> Bool)]
  -> [Ly.PostFilterFn]
  -> [L.Box Ly.LibertyMeta]
  -> Maybe [L.Box Ly.LibertyMeta]
filterBoxes tks pfs bs = fmap fltr (Ly.parsePredicate tks)
  where
    fltr p =
      Ly.processPostFilters pfs . filter p $ bs



-- | Applied to a list of Box that have already been filtered, returns
-- a list of Box with posting metadata.
addMetadata ::
  CO.ShowZeroBalances
  -> [L.Box Ly.LibertyMeta]
  -> [L.Box PostMeta]
addMetadata szb = M.visibleNums f . addBalances szb where
  f vn (lm, mb) =
    PostMeta (Ly.filteredNum lm) (Ly.sortedNum lm) vn mb

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

