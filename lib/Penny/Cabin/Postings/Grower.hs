-- | Step 8 - GrowToFit. See "Penny.Cabin.Postings.Grid" for more
-- details on where this fits into the larger order.
module Penny.Cabin.Postings.Grower where

import qualified Data.Array as A
import Data.List (intersperse)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Meta as Me
import qualified Penny.Lincoln.Queries as Q
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

type Grower a =
  O.Options a
  -> A.Array Col C.Width
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe T.ClaimedWidth)
  -> Maybe R.Cell

grower :: Grower a
grower o w (c, (vn, r)) p =
  f o w (c, (vn, r)) p where
    f = growers ! (c, r)

padding :: Grower a
padding opts w (col, (vn, _)) _ = let
  ts = PC.colors vn (O.baseColors opts)
  width = w A.! col
  j = R.LeftJustify
  sq = Seq.empty
  in Just $ R.Cell j width ts sq

emptyButPadded :: Grower a
emptyButPadded = padding

overran :: Grower a
overran _ _ _ _ = Just R.zeroCell

allocated :: Grower a
allocated _ _ _ _ = Nothing

overrunning :: Grower a
overrunning _ _ _ _ = Nothing

ifShown ::
  O.Options a
  -> (F.Fields Bool -> Bool)
  -> A.Array Col C.Width
  -> Col
  -> R.Justification
  -> C.TextSpec
  -> Seq.Seq C.Chunk
  -> R.Cell
ifShown opts fn wa c just ts cs = let
  w = wa A.! c in
  if fn . O.fields $ opts
  then R.Cell just w ts cs
  else R.Cell just w ts Seq.empty


lineNum :: Grower a
lineNum os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.lineNum wa col R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.postingLine . T.postingBox $ p of
      Nothing -> Seq.empty
      (Just ln) -> Seq.singleton . C.chunk ts
                   . X.pack . show . Me.unLine
                   . Me.unPostingLine $ ln

date :: Grower a
date os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.date wa col R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = Seq.singleton . C.chunk ts . O.dateFormat os $ p

surround :: Char -> Char -> X.Text -> X.Text
surround l r t = l `X.cons` t `X.snoc` r

flag :: Grower a
flag os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.flag wa col R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.flag . T.postingBox $ p of
      Nothing -> Seq.empty
      Just fl -> Seq.singleton
                 . C.chunk ts
                 . surround '[' ']'
                 . HT.text
                 $ fl
number :: Grower a
number os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.number wa col R.LeftJustify ts cs where
    ts = PC.colors vn (O.baseColors os)
    cs = case Q.number . T.postingBox $ p of
      Nothing -> Seq.empty
      Just fl -> Seq.singleton
                 . C.chunk ts
                 . surround '(' ')'
                 . HT.text
                 $ fl

postingDrCr :: Grower a
postingDrCr os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.postingDrCr wa col R.LeftJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = Seq.singleton
         . C.chunk ts
         . X.pack
         $ case dc of
           Bits.Debit -> "Dr"
           Bits.Credit -> "Cr"

postingCmdty :: Grower a
postingCmdty os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.postingCmdty wa col R.RightJustify ts cs where
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

postingQty :: Grower a
postingQty os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.postingQty wa col R.RightJustify ts cs where
    ts = PC.colors vn bc
    bc = PC.drCrToBaseColors dc (O.drCrColors os)
    dc = Q.drCr . T.postingBox $ p
    cs = Seq.singleton
         . C.chunk ts
         . O.qtyFormat os
         $ p

totalDrCr :: Grower a
totalDrCr os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.totalDrCr wa col R.LeftJustify ts cs where
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
        . PC.bottomLineToBaseColors (O.drCrColors os)
        $ nou
      txt = X.pack $ case nou of
        Bal.Zero -> "--"
        Bal.NonZero clm -> case Bal.drCr clm of
          Bits.Debit -> "Dr"
          Bits.Credit -> "Cr"
      in C.chunk spec txt

totalCmdty :: Grower a
totalCmdty os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.totalCmdty wa col R.RightJustify ts cs where
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
        . PC.bottomLineToBaseColors (O.drCrColors os)
        $ nou
      txt = HT.text
            . HT.Delimited (X.singleton ':')
            . HT.textList
            $ com
      in C.chunk spec txt

totalQty :: Grower a
totalQty os wa (col, (vn, _)) (p, _) =
  Just $ ifShown os F.totalQty wa col R.LeftJustify ts cs where
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
        . PC.bottomLineToBaseColors (O.drCrColors os)
        $ nou
      txt = O.balanceFormat os com nou
      in C.chunk spec txt

topRow :: [(Address, Grower a)]
topRow = zipWith tup cols ls where
  tup col g = ((col, Adr.Top), g)
  cols = [minBound..maxBound]
  ls = intersperse padding
       [ lineNum, date, flag, number, allocated, allocated,
         postingDrCr, postingCmdty, postingQty,
         totalDrCr, totalCmdty, totalQty ]

otherRows :: [(Address, Grower a)]
otherRows = lEmpties ++ oruns ++ orans ++ rEmpties where
  makeList uL lR g = zip (A.range (b, e)) (repeat g) where
    b = (uL, Adr.Tags)
    e = (lR, Adr.Filename)
  lEmpties = makeList Adr.LineNum Adr.SDate emptyButPadded
  oruns = makeList Adr.Multi Adr.Multi overrunning
  orans = makeList Adr.SMulti Adr.PostingQty overran
  rEmpties = makeList Adr.SPostingQty Adr.TotalQty emptyButPadded

growers :: M.Map Address (Grower a)
growers = M.fromList (topRow ++ otherRows)
