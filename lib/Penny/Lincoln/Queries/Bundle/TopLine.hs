module Penny.Lincoln.Queries.Bundle.TopLine where

import Penny.Lincoln.Transaction
import qualified Penny.Lincoln.TopLine as T
import Penny.Lincoln.Common
import Penny.Lincoln.Pieces
import Penny.Lincoln.Serial

topLineData :: Bundle -> T.TopLineData
topLineData = T.tlData . bunTopLine

topLineMeta :: Bundle -> Maybe T.TopLineMeta
topLineMeta = T.tlMeta . bunTopLine

dateTime :: Bundle -> DateTime
dateTime = T.tlDateTime . topLineData

memo :: Bundle -> Memo
memo = T.tlMemo . topLineData

number :: Bundle -> Number
number = T.tlNumber . topLineData

flag :: Bundle -> Flag
flag = T.tlFlag . topLineData

payee :: Bundle -> Payee
payee = T.tlPayee . topLineData

topMemoLine :: Bundle -> Maybe Line
topMemoLine = fmap T.tlmTopMemo . topLineMeta

topLineLine :: Bundle -> Maybe Line
topLineLine = fmap T.tlmTopLine . topLineMeta

globalTransaction :: Bundle -> Maybe Serial
globalTransaction = fmap T.tlmGlobalTransaction . topLineMeta

fileTransaction :: Bundle -> Maybe Serial
fileTransaction = fmap T.tlmFileTransaction . topLineMeta

filename :: Bundle -> Maybe Filename
filename = fmap T.tlmFilename . topLineMeta
