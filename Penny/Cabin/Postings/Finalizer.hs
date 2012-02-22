module Penny.Cabin.Postings.Finalizer where

import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.Sequence as Seq
import qualified Data.Table as Tb
import qualified Data.Text as X

import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

import qualified Penny.Cabin.Allocate as Alo
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Index = (Col, (T.VisibleNum, Row))
type Address = (Col, Row)

type Finalizer =
  F.Fields Bool
  -> O.Options
  -> Arr
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe R.Cell)
  -> R.Cell

finalizer :: Finalizer
finalizer flds os a (col, (vn, r)) (p, mc) = case mc of
  Just c -> c
  Nothing -> let f fn = fn flds os a (col, (vn, r)) (p, mc) in
    case (col, r) of
      (Adr.Multi, Adr.Tags) -> f tags
      (Adr.Multi, Adr.Memo) -> f memo
      (Adr.Multi, Adr.Filename) -> f filename
      _ -> f overran

concatRange ::
  (Monoid w, A.Ix i)
  => (i, i)
  -> A.Array i w
  -> w
concatRange r a = mconcat cs where
  cs = map ((A.!) a) (A.range r)

rangeAdd :: (Index, Index)
            -> Arr
            -> C.Width
rangeAdd ip a = concatRange ip a' where
  a' = fmap f a
  f mc = case snd mc of
    Nothing -> mempty
    (Just cell) -> R.width cell

widthFlagToPostingQty ::
  T.VisibleNum
  -> Arr
  -> C.Width
widthFlagToPostingQty vn = rangeAdd i where
  r = (vn, Adr.Top)
  i = ((Adr.Multi, r), (Adr.PostingQty, r))

tags :: Finalizer
tags flds os a (col, (vn, r)) (p, mc) = cell where
  cell = R.Cell R.LeftJustify w ts cs
  w = widthFlagToPostingQty vn a
  ts = PC.colors vn (O.baseColors os)
  cs =
    fmap toChunk
    . TF.unLines
    . TF.wordWrap (C.unWidth w)
    . TF.Words
    . Seq.fromList
    . map (X.cons '#')
    . HT.textList
    . Q.tags
    . T.postingBox
    $ p
  toChunk (TF.Words ws) = C.chunk ts t where
    t = X.concat . intersperse (X.singleton ' ') . F.toList $ ws
              
memo = undefined
filename = undefined
overran = undefined

