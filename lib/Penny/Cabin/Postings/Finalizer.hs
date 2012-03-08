-- | Step 10 - Finalizer. See the comments in
-- "Penny.Cabin.Postings.Grid" to see where this fits in.
module Penny.Cabin.Postings.Finalizer where

import qualified Data.Array as A
import qualified Data.Foldable as Fd
import Data.List (intersperse)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.Meta as Me

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
  O.Options
  -> Arr
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe R.Cell)
  -> R.Cell

finalizer :: Finalizer
finalizer os a (col, (vn, r)) (p, mc) = case mc of
  Just c -> c
  Nothing -> let f fn = fn os a (col, (vn, r)) (p, mc) in
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
tags os a (_, (vn, _)) (p, _) = cell where
  cell = if F.tags . O.fields $ os
         then R.Cell R.LeftJustify w ts cs
         else R.zeroCell
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
    t = X.concat . intersperse (X.singleton ' ') . Fd.toList $ ws
              
memoChunks :: C.TextSpec -> Bits.Memo -> C.Width -> Seq.Seq C.Chunk
memoChunks ts m (C.Width w) = cs where
  cs = fmap toChunk
       . TF.unLines
       . TF.wordWrap w
       . TF.Words
       . Seq.fromList
       . X.words
       . HT.text
       $ m
  toChunk (TF.Words ws) = C.chunk ts (X.unwords . Fd.toList $ ws)

memo :: Finalizer
memo os a (_, (vn, _)) (p, _) = cell where
  cell = if F.memo . O.fields $ os
         then R.Cell R.LeftJustify w ts cs
         else R.zeroCell
  pm = Q.postingMemo . T.postingBox $ p
  tm = Q.transactionMemo . T.postingBox $ p
  cs = case (pm, tm) of
    (Nothing, Nothing) -> mempty
    (Just pms, Nothing) -> memoChunks ts pms w
    (Nothing, Just tms) -> memoChunks ts tms w
    (Just pms, Just tms) ->
      memoChunks ts pms w `mappend` memoChunks ts tms w
  w = widthFlagToPostingQty vn a
  ts = PC.colors vn (O.baseColors os)
  
filename :: Finalizer
filename os a (_, (vn, _)) (p, _) = cell where
  cell = if F.filename . O.fields $ os
         then R.Cell R.LeftJustify w ts cs
         else R.zeroCell
  w = widthFlagToPostingQty vn a
  toChunk n = C.chunk ts
              . X.drop (max 0 (C.unWidth w - X.length n)) $ n
  cs = case Q.filename . T.postingBox $ p of
    Nothing -> Seq.empty
    Just fn -> Seq.singleton . toChunk . Me.unFilename $ fn
  ts = PC.colors vn (O.baseColors os)

overran :: Finalizer
overran _ _ _ _ = R.zeroCell

