module Penny.Liberty.Seq where

import Control.Applicative ((<|>))
import Data.List (sortBy)
import Data.Text (pack)
import System.Console.MultiArg.Prim (ParserE)
import System.Console.MultiArg.Combinator (longTwoArg)
import System.Console.MultiArg.Option (makeLongOpt)

import qualified Penny.Liberty.Expressions as X
import qualified Penny.Liberty.Operands as O
import qualified Penny.Liberty.Types as T
import qualified Penny.Liberty.Sorter as S
import Penny.Liberty.Error (Error)

import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Predicates as P

postingInfos :: S.Orderer -> [PostingBox] -> [T.PostingInfo]
postingInfos o = numberSorted . sortTriples o . numberUnsorted

numberUnsorted ::
  [PostingBox]
  -> [(PostingBox, T.FwdSeqUnsorted, T.BackSeqUnsorted)]
numberUnsorted ps = reverse backSeqs where
  fwdSeqs = zipWith (,) ps (map T.FwdSeqUnsorted [0..])
  backSeqs = zipWith f (reverse fwdSeqs) bs where
    bs = (map T.BackSeqUnsorted [0..])
    f (pb, fu) bu = (pb, fu, bu)

sortTriples ::
  S.Orderer
  -> [(PostingBox, T.FwdSeqUnsorted, T.BackSeqUnsorted)]
  -> [(PostingBox, T.FwdSeqUnsorted, T.BackSeqUnsorted)]
sortTriples o ps = sortBy f ps where
  f (pb1, _, _) (pb2, _, _) = o pb1 pb2

numberSorted ::
  [(PostingBox, T.FwdSeqUnsorted, T.BackSeqUnsorted)]
  -> [T.PostingInfo]
numberSorted ps = reverse backNums where
  fwdNums = zipWith f ps (map T.FwdSeqSorted [0..]) where
    f (pb, fu, bu) fs = (pb, fu, bu, fs)
  backNums = zipWith f (reverse fwdNums) bss where
    f (pb, fu, bu, fs) bs = T.PostingInfo pb fu bu fs bs
    bss = map T.BackSeqSorted [0..]

type Operand = X.Token (T.PostingInfo -> Bool)

fwdSeqUnsorted :: ParserE Error Operand
fwdSeqUnsorted = seqOption "fwd-seq-unsorted" T.FwdSeqUnsorted
                 T.testFwdSeqUnsorted 

fwdSeqSorted :: ParserE Error Operand
fwdSeqSorted = seqOption "fwd-seq-sorted" T.FwdSeqSorted
                 T.testFwdSeqSorted 

backSeqUnsorted :: ParserE Error Operand
backSeqUnsorted = seqOption "back-seq-unsorted" T.BackSeqUnsorted
                 T.testBackSeqUnsorted 

backSeqSorted :: ParserE Error Operand
backSeqSorted = seqOption "back-seq-sorted" T.BackSeqSorted
                 T.testBackSeqSorted 

seqOption ::
  String
  -- ^ Long option name
  
  -> (Integer -> a)
  -- ^ Function to make the sequence type

  -> (P.Comparer -> a -> T.PostingInfo -> Bool)
  -- ^ Function to make the tester

  -> ParserE Error Operand
seqOption str fs ft = do
  let lo = makeLongOpt . pack $ str
  (_, cStr, iStr) <- longTwoArg lo
  cmp <- O.throwIf $ O.parseComparer cStr
  i <- O.throwIf $ O.parseInt iStr
  let n = fs . fromIntegral $ i
  return $ X.TokOperand (ft cmp n)

parser :: ParserE Error Operand
parser = fwdSeqUnsorted <|> fwdSeqSorted
         <|> backSeqUnsorted <|> backSeqSorted
