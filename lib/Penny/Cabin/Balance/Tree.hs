-- | Takes postings and places them into a tree for further
-- processing.
module Penny.Cabin.Balance.Tree where

import qualified Penny.Cabin.Row as R
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
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

fillTextSpec ::
  O.Options
  -> IsEven
  -> Chunk.TextSpec
fillTextSpec os isEven = let
  getTs = if isEven then C.evenColors else C.oddColors
  in getTs . O.baseColors $ os
  


makeQtyCell ::
  O.Options
  -> S.Option Bal.Balance
  -> Chunk.TextSpec
  -> IsEven
  -> R.Cell
makeQtyCell os mayBal fill isEven = R.Cell j w fill cs where
  j = R.RightJustify
  w = Chunk.Width 0
  cs = case S.getOption mayBal of
    Nothing -> let
      getTs = if isEven then C.evenZero else C.oddZero
      ts = getTs . O.drCrColors $ os
      in Seq.singleton (Chunk.chunk ts (X.pack "--"))
    Just bal -> undefined

bottomLineCells ::
  O.Options
  -> IsEven
  -> S.Option Bal.Balance
  -> (R.Cell, R.Cell, R.Cell)
bottomLineCells os isEven mayBal = let
  fill = fillTextSpec os isEven
  tsZero = if isEven
           then C.evenZero . O.drCrColors $ os
           else C.oddZero . O.drCrColors $ os
  zeroCell =
    R.Cell R.LeftJustify (Chunk.Width 0)
    tsZero (Seq.singleton (Chunk.chunk tsZero (X.pack "--")))
  zeroCells = (zeroCell, zeroCell, zeroCell)
  in case S.getOption mayBal of
    Nothing -> zeroCells
    Just bal -> bottomLineBalCells fill
                . map (bottomLineBalChunks os isEven)
                . M.assocs
                . Bal.unBalance
                $ bal
  
            

-- | Takes a list of triples from bottomLineChunks and creates three
-- Cells, one each for DrCr, Commodity, and Qty.
bottomLineBalCells ::
  Chunk.TextSpec -- ^ Fill colors
  -> [(Chunk.Chunk, Chunk.Chunk, Chunk.Chunk)]
  -> (R.Cell, R.Cell, R.Cell)
bottomLineBalCells spec ts = (mkCell dc, mkCell ct, mkCell qt) where
  mkCell sq = R.Cell R.LeftJustify (Chunk.Width 0) spec sq
  e = Seq.empty
  (dc, ct, qt) = Fdbl.foldl' f (e, e, e) ts
  f (da, ca, qa) (d, c, q) = (da |> d, ca |> c, qa |> q)

-- | Returns a triple (x, y, z), where x is the DrCr chunk, y is the
-- commodity chunk, and z is the qty chunk.
bottomLineBalChunks ::
  O.Options
  -> IsEven
  -> (L.Commodity, Bal.BottomLine)
  -> (Chunk.Chunk, Chunk.Chunk, Chunk.Chunk)
bottomLineBalChunks os isEven (comm, bl) = (dc, cty, qty) where
  dc = Chunk.chunk ts dcTxt
  cty = Chunk.chunk ts ctyTxt
  qty = Chunk.chunk ts qtyTxt
  ctyTxt = L.text (L.Delimited (X.singleton ':') (L.textList comm))
  (ts, dcTxt, qtyTxt) = case bl of
    Bal.Zero -> let
      getTs = if isEven then C.evenZero else C.oddZero
      dcT = X.pack "--"
      qtyT = dcT
      in (getTs . O.drCrColors $ os, dcT, qtyT)
    Bal.NonZero clm -> let
      (getTs, dcT) = case Bal.drCr clm of
        L.Debit ->
          (if isEven then C.evenDebit else C.oddDebit,
           X.pack "Dr")
        L.Credit ->
          (if isEven then C.evenCredit else C.evenCredit,
           X.pack "Cr")
      qTxt = (O.balanceFormat os) bl
      in (getTs . O.drCrColors $ os, dcT, qTxt)

{-

makeCommodityCell ::
  S.Option Bal.Balance
  -> Chunk.TextSpec
  -> R.Cell
makeCommodityCell mayBal ts = R.Cell j w ts cs where
  j = R.LeftJustify
  w = Chunk.Width 0
  csTxt = case S.getOption mayBal of
    Nothing -> Seq.singleton . X.pack $ "--"
-}
