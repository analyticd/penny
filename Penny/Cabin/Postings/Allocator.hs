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

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Meta as Me
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
payee flds os a (col, (vn, r)) (p, _) =
  case widths flds os vn a of
    (Just pw, _) -> Just $ payeeCell pw p ts where
      ts = PC.colors vn (O.baseColors os)
    (Nothing, _) -> Nothing

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
  widths = Alo.allocate allocs
           (fromIntegral . unMaxAllocated $ widthMax)
  payee = PayeeWidth . fromIntegral . (! 'p') $ widths
  acct = AccountWidth . fromIntegral . (! 'a') $ widths
  payeeAll = PayeeWidth . fromIntegral . unMaxAllocated $ widthMax
  acctAll = AccountWidth . fromIntegral . unMaxAllocated $ widthMax
  (pw, aw) = case (F.payee flds, F.account flds) of
    (True, True) -> (Just payee, Just acct)
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
