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
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Address = (Col, Row)

type Allocator =
  F.Fields Bool
  -> O.Options
  -> Arr
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe R.Cell)
  -> Maybe R.Cell

newtype PayeeWidth = PayeeWidth { unPayeeWidth :: Int }
                     deriving (Show, Eq, Ord)

newtype AccountWidth = AccountWidth { unAccountWidth :: Int }
                       deriving (Show, Eq, Ord)

allocator :: Allocator
allocator flds os a (col, (vn, r)) (p, mc) = case mc of
  Just c -> Just c
  Nothing -> case (col, r) of
    (Adr.Payee, Adr.Top) -> payee flds os a (col, (vn, r)) (p, mc)
    (Adr.Account, Adr.Top) -> account flds os a (col, (vn, r)) (p, mc)
    _ -> Nothing

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

payee :: Allocator
payee flds os a (_, (vn, _)) (p, _) =
  case fst $ widths flds os vn a of
    Just pw -> Just $ payeeCell pw p ts where
      ts = PC.colors vn (O.baseColors os)
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


account :: Allocator
account flds os a (_, (vn, _)) (p, _) =
  case snd $ widths flds os vn a of
    Just aw -> Just $ accountCell aw os p ts where
      ts = PC.colors vn (O.baseColors os)
    Nothing -> Nothing

widths :: F.Fields Bool
          -> O.Options
          -> T.VisibleNum
          -> Arr
          -> (Maybe PayeeWidth, Maybe AccountWidth)
widths flds os vn arr = (pw, aw) where
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
    Nothing -> mempty
    (Just c) -> R.width c

newtype MaxAllocated = MaxAllocated { unMaxAllocated :: Int }
                       deriving (Eq, Ord, Show)

maxAllocatedWidth :: C.Width -> O.ReportWidth -> MaxAllocated
maxAllocatedWidth (C.Width rowW) (O.ReportWidth reportW) =
  MaxAllocated $ max 0 (reportW - rowW)
