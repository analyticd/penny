-- | Takes postings and places them into a tree for further
-- processing.
module Penny.Cabin.Balance.Tree where

import qualified Data.Foldable as Fdbl
import qualified Data.NestedMap as NM
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Balance as Bal
import qualified Data.Semigroup as S

type Balances = NM.NestedMap L.SubAccountName (S.Option Bal.Balance)

addPosting :: Balances -> (L.Account, L.Entry) -> Balances
addPosting bals (ac, en) = let
  bal = S.Option . Just . L.entryToBalance $ en
  subs = Fdbl.toList . L.unAccount $ ac
  in NM.insert bals subs bal

balances :: [LT.PostingInfo] -> Balances
balances = Fdbl.foldl' addPosting NM.empty . map toPair where
  toPair p = let
    box = LT.postingBox p
    ac = Q.account box
    en = Q.entry box
    in (ac, en)
