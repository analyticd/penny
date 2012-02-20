module Penny.Cabin.Postings.Grower where

import Control.Applicative
  ((<$>), (<*>), pure, ZipList(ZipList, getZipList))
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import qualified Data.Table as Tb
import qualified Data.Text as X

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Meta as Me
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.HasText (text)
import qualified Penny.Lincoln.HasText as HT

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe T.ClaimedWidth)

type Address = (Col, Row)

type Grower =
  F.Fields Bool
  -> O.Options
  -> Arr
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe T.ClaimedWidth)
  -> Maybe R.Cell

widest :: Col -> Arr -> C.Width
widest col arr = F.foldr f (C.Width 0) clm where
  clm = Tb.OneDim . Tb.column arr $ col
  f c soFar = max width soFar where
    width = case snd c of
      Nothing -> C.Width 0
      (Just cw) ->
        C.Width
        . T.unClaimedWidth
        $ cw

padding :: Grower
padding _ opts a (col, (vn, _)) _ = let
  ts = PC.colors vn (O.baseColors opts)
  width = widest col a
  j = R.LeftJustify
  sq = Seq.empty
  in Just $ R.Cell j width ts sq

emptyButPadded :: Grower
emptyButPadded = padding

overran :: Grower
overran _ _ _ _ _ = Just R.zeroCell

allocated :: Grower
allocated _ _ _ _ _ = Nothing

overrunning :: Grower
overrunning _ _ _ _ _ = Nothing

ifShown ::
  F.Fields Bool
  -> (F.Fields Bool -> Bool)
  -> Col
  -> Arr
  -> R.Justification
  -> C.TextSpec
  -> Seq.Seq C.Chunk
  -> R.Cell
ifShown flds fn c a just ts cs = let
  w = widest c a in
  if fn flds
  then R.Cell just w ts cs
  else R.Cell just w ts Seq.empty


lineNum :: Grower
lineNum flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.lineNum col a R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.postingLine . T.postingBox $ p of
      Nothing -> Seq.empty
      (Just ln) -> Seq.singleton . C.chunk ts
                   . X.pack . show . Me.unLine
                   . Me.unPostingLine $ ln

date :: Grower
date flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.date col a R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = Seq.singleton . C.chunk ts . O.dateFormat os $ p

surround :: Char -> Char -> X.Text -> X.Text
surround l r t = l `X.cons` t `X.snoc` r

flag :: Grower
flag flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.date col a R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.flag . T.postingBox $ p of
      Nothing -> Seq.empty
      Just fl -> Seq.singleton
                 . C.chunk ts
                 . surround '[' ']'
                 . HT.text
                 $ fl
number :: Grower
number flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.number col a R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.number . T.postingBox $ p of
      Nothing -> Seq.empty
      Just fl -> Seq.singleton
                 . C.chunk ts
                 . surround '(' ')'
                 . HT.text
                 $ fl

postingDrCr :: Grower
postingDrCr flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.postingDrCr col a R.LeftJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = Seq.singleton
         . C.chunk ts
         . X.pack
         $ case dc of
           Bits.Debit -> "Dr"
           Bits.Credit -> "Cr"

postingCmdty :: Grower
postingCmdty flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.postingCmdty col a R.RightJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = Seq.singleton
         . C.chunk ts
         . HT.text
         . HT.Delimited (X.singleton ':')
         . HT.textList
         . Q.commodity
         . T.postingBox
         $ p

postingQty :: Grower
postingQty flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.postingQty col a R.RightJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = Seq.singleton
         . C.chunk ts
         . O.qtyFormat os
         $ p

totalDrCr :: Grower
totalDrCr flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.totalDrCr col a R.LeftJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = fmap toChunk
         . Seq.fromList
         . M.elems
         . Bal.unBalance
         . T.balance
         $ p
    toChunk nou = let
      spec = 
        PC.colors vn
        . PC.noughtToBaseColors (O.drCrColors os)
        $ nou
      txt = X.pack $ case nou of
        Bal.Zero -> "--"
        Bal.NonZero clm -> case Bal.drCr clm of
          Bits.Debit -> "Dr"
          Bits.Credit -> "Cr"
      in C.chunk spec txt

totalCmdty :: Grower
totalCmdty flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.totalCmdty col a R.RightJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = fmap toChunk
         . Seq.fromList
         . M.assocs
         . Bal.unBalance
         . T.balance
         $ p
    toChunk (com, nou) = let
      spec =
        PC.colors vn
        . PC.noughtToBaseColors (O.drCrColors os)
        $ nou
      txt = HT.text
            . HT.Delimited (X.singleton ':')
            . HT.textList
            $ com
      in C.chunk spec txt

totalQty :: Grower
totalQty flds os a (col, (vn, r)) (p, _) =
  Just $ ifShown flds F.totalQty col a R.LeftJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = fmap toChunk
         . Seq.fromList
         . M.assocs
         . Bal.unBalance
         . T.balance
         $ p
    toChunk (com, nou) = let
      spec = 
        PC.colors vn
        . PC.noughtToBaseColors (O.drCrColors os)
        $ nou
      txt = O.balanceFormat os com nou
      in C.chunk spec txt

topRow :: [(Address, Grower)]
topRow = zipWith tup cols ls where
  tup col g = ((col, Adr.Top), g)
  cols = [minBound..maxBound]
  ls = intersperse padding
       [ lineNum, date, flag, number, allocated, allocated,
         postingDrCr, postingCmdty, postingQty,
         totalDrCr, totalCmdty, totalQty ]
