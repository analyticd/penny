-- | Step 10 - Allocation. See "Penny.Cabin.Postings.Grid" for more
-- details on what is happening in here.
module Penny.Cabin.Postings.Allocator where

import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Address = (Col, Row)

allocator :: O.Options -> G.Allocator Col Row
allocator os m (col, (vn, r)) (p, mc) = case mc of
  Just c -> Just c
  Nothing -> case r of
    Adr.Top -> Just $ let
      ts = PC.colors vn (O.baseColors os)
      in case col of
        Adr.Payee -> let
          w = m M.! Adr.Payee
          in payeeCell w p ts
        Adr.Account -> let
          w = m M.! Adr.Account
          in accountCell w os p ts
        _ -> error "allocator error: should never happen"
    _ -> Nothing

payeeCell ::
  C.Width
  -> T.PostingInfo
  -> C.TextSpec
  -> R.Cell
payeeCell (C.Width pw) p ts =
  if pw == 0 then R.zeroCell else
    R.Cell R.LeftJustify (C.Width pw) ts $
    case Q.payee . T.postingBox $ p of
      Nothing -> Seq.empty
      Just pye -> let
        wrapped = TF.unLines 
                  . TF.wordWrap pw
                  . TF.txtWords
                  . HT.text
                  $ pye
        toChunk (TF.Words seqTxts) =
          C.chunk ts
          . X.unwords
          . F.toList
          $ seqTxts
        in fmap toChunk wrapped

accountCell ::
  C.Width
  -> O.Options
  -> T.PostingInfo
  -> C.TextSpec
  -> R.Cell
accountCell (C.Width aw) os p ts =
  if aw == 0 then R.zeroCell else
    R.Cell R.LeftJustify (C.Width aw) ts $ let
    target = TF.Target aw
    shortest = TF.Shortest . O.subAccountLength $ os
    a = Q.account . T.postingBox $ p
    ws = TF.Words . Seq.fromList . HT.textList $ a
    (TF.Words shortened) = TF.shorten shortest target ws
    in Seq.singleton
       . C.chunk ts
       . X.concat
       . intersperse (X.singleton ':')
       . F.toList
       $ shortened



{-
allocator ::  O.Options -> G.Allocator Col Row
allocator os a (col, (vn, r)) (p, ac) = case ac of
  Just c -> Just c
  Nothing -> case r of
    Adr.Top -> Just $ case col of
      Adr.Payee -> payeeCell 
  G.AcCell c -> Just c
  G.AcOverrunning -> Nothing
  G.AcWidth _ -> case (col, r) of
    (Adr.Payee, Adr.Top) -> 
    (Adr.Account, Adr.Top) -> account os a (col, (vn, r)) (p, ac)
    _ -> error "allocator 1 error: should never happen"

payee :: O.Options -> G.Allocator Col Row
payee os a (_, (vn, _)) (p, _) =
  case fst $ aloWidths os vn a of
    Just pw -> Just $ payeeCell bestWidth p ts where
      ts = PC.colors vn (O.baseColors os)
      bestWidth =
        PayeeWidth
        $ min (unPayeeWidth pw)
        (maxPayeeReservedWidth a)
    Nothing -> Nothing

-- | Examines the array and allocates the maximum amount of space that
-- this cell could use. THe maximum amount that can be used is the
-- minimum of:
--
-- * the allocated width of this column, or
--
-- * the widest cell in the column.
account ::  O.Options -> G.Allocator Col Row
account os a (_, (vn, _)) (p, _) =
  case snd $ aloWidths os vn a of
    Just aw -> Just $ accountCell bestWidth os p ts where
      ts = PC.colors vn (O.baseColors os)
      bestWidth = AccountWidth
                  $ min (unAccountWidth aw)
                  (maxAccountReservedWidth a)
    Nothing -> Nothing



-}
