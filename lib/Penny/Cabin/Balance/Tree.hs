-- | Takes postings and places them into a tree for further
-- processing.
module Penny.Cabin.Balance.Tree where

import qualified Penny.Cabin.Row as R
import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import qualified Data.NestedMap as NM
import qualified Data.Text as X
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
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

type IsEven = Bool

data Columns a = Columns {
  account :: a
  , drCr :: a
  , commodity :: a
  , quantity :: a
  } deriving Show

makeRow ::
  O.Options
  -> [(L.SubAccountName, S.Option Bal.Balance)]
  -> L.SubAccountName
  -> S.Option Bal.Balance
  -> IsEven
  -> Columns R.Cell
makeRow os ps a mayBal isEven = undefined


makeQtyCell ::
  S.
  -> S.Option Bal.Balance
  -> IsEven
  -> R.Cell
makeQtyCell mayBal ts = R.Cell j w ts cs where
  j = R.RightJustify
  w = Chunk.Width 0
  csTxt = case S.getOption mayBal of
    Nothing -> Seq.singleton . X.pack $ "--"
    Just bal -> Seq.fromList . fmap (O.balanceFormat o)
                . M.elems . Bal.unBalance $ bal
  cs = fmap (Chunk.chunk ts) csTxt

makeCommodityCell ::
  S.Option Bal.Balance
  -> Chunk.TextSpec
  -> R.Cell
makeCommodityCell mayBal ts = R.Cell j w ts cs where
  j = R.LeftJustify
  w = Chunk.Width 0
  csTxt = case S.getOption mayBal of
    Nothing -> Seq.singleton . X.pack $ "--"
