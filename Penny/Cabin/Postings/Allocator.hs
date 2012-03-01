-- | Step 9 - Allocation. See "Penny.Cabin.Postings.Grid" for more
-- details on what is happening in here.
module Penny.Cabin.Postings.Allocator where

import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Monoid (mempty, mappend)
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
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, G.AcClaim)

type Address = (Col, Row)

newtype PayeeWidth = PayeeWidth { unPayeeWidth :: Int }
                     deriving (Show, Eq, Ord)

newtype AccountWidth = AccountWidth { unAccountWidth :: Int }
                       deriving (Show, Eq, Ord)

allocator ::  F.Fields Bool -> O.Options -> G.Allocator Col Row
allocator flds os a (col, (vn, r)) (p, ac) = case ac of
  G.AcCell c -> Just c
  G.AcOverrunning -> Nothing
  G.AcWidth _ -> case (col, r) of
    (Adr.Payee, Adr.Top) -> payee flds os a (col, (vn, r)) (p, ac)
    (Adr.Account, Adr.Top) -> account flds os a (col, (vn, r)) (p, ac)
    _ -> error "allocator 1 error: should never happen"

payeeCell ::
  PayeeWidth
  -> T.PostingInfo
  -> C.TextSpec
  -> R.Cell
payeeCell (PayeeWidth pw) p ts =
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

payee :: F.Fields Bool -> O.Options -> G.Allocator Col Row
payee flds os a (_, (vn, _)) (p, _) =
  case fst $ aloWidths flds os vn a of
    Just pw -> Just $ payeeCell bestWidth p ts where
      ts = PC.colors vn (O.baseColors os)
      bestWidth =
        PayeeWidth
        $ min (unPayeeWidth pw)
        (maxPayeeReservedWidth a)
    Nothing -> Nothing

accountCell ::
  AccountWidth
  -> O.Options
  -> T.PostingInfo
  -> C.TextSpec
  -> R.Cell
accountCell (AccountWidth aw) os p ts =
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


account ::  F.Fields Bool -> O.Options -> G.Allocator Col Row
account flds os a (_, (vn, _)) (p, _) =
  case snd $ aloWidths flds os vn a of
    Just aw -> Just $ accountCell bestWidth os p ts where
      ts = PC.colors vn (O.baseColors os)
      bestWidth = AccountWidth
                  $ min (unAccountWidth aw)
                  (maxAccountReservedWidth a)
    Nothing -> Nothing

maxPayeeReservedWidth :: Arr -> Int
maxPayeeReservedWidth a = F.foldr folder 0 clm where
  clm = Tb.column a Adr.Payee
  folder (info, ac) maxSoFar = case ac of
    G.AcWidth w -> max maxSoFar w where
      w = case Q.payee . T.postingBox $ info of
        Nothing -> 0
        (Just pye) -> X.length . HT.text $ pye
    _ -> maxSoFar

maxAccountReservedWidth :: Arr -> Int
maxAccountReservedWidth a = F.foldr folder 0 clm where
  clm = Tb.column a Adr.Account
  folder (info, ac) maxSoFar = case ac of
    G.AcWidth w -> max maxSoFar w where
        w = X.length . HT.text . HT.Delimited (X.singleton ':')
            . HT.textList . Q.account . T.postingBox $ info
    _ -> maxSoFar


aloWidths :: F.Fields Bool
          -> O.Options
          -> T.VisibleNum
          -> Arr
          -> (Maybe PayeeWidth, Maybe AccountWidth)
aloWidths flds os vn arr = (pw, aw) where
  allocs = M.fromList [ ('p', O.payeeAllocation os)
                      , ('a', O.accountAllocation os) ]
  widthTop = topRowWidth vn arr
  widthMax = maxAllocatedWidth widthTop (O.width os)
  ws = Alo.allocate allocs
           (fromIntegral . unMaxAllocated $ widthMax)
  pay = PayeeWidth . fromIntegral . (! 'p') $ ws
  acct = AccountWidth . fromIntegral . (! 'a') $ ws
  payeeAll = PayeeWidth . fromIntegral . unMaxAllocated $ widthMax
  acctAll = AccountWidth . fromIntegral . unMaxAllocated $ widthMax
  (pw, aw) = case (F.payee flds, F.account flds) of
    (True, True) -> (Just pay, Just acct)
    (False, False) -> (Nothing, Nothing)
    (True, False) -> (Just payeeAll, Nothing)
    (False, True) -> (Nothing, Just acctAll)

topRowWidth :: T.VisibleNum -> Arr -> C.Width
topRowWidth vn a = F.foldr mappend mempty r where
  r = fmap toWidth . Tb.OneDim . Tb.row a $ (vn, Adr.Top)
  toWidth (_, cell) = case cell of
    G.AcCell c -> R.width c
    _ -> mempty

newtype MaxAllocated = MaxAllocated { unMaxAllocated :: Int }
                       deriving (Eq, Ord, Show)

maxAllocatedWidth :: C.Width -> O.ReportWidth -> MaxAllocated
maxAllocatedWidth (C.Width rowW) (O.ReportWidth reportW) =
  MaxAllocated $ max 0 (reportW - rowW)
