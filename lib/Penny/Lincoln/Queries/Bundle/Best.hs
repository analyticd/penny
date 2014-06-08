-- | Queries that return the \"best\" field for a bundle.  In cases
-- where the desired field is also available in the TopLine, the query
-- first examines the field in the viewed posting.  If that field is
-- not empty, its value is returned; otherwise, the field in the
-- TopLine is returned.  For fields where the desired field is not in
-- the TopLine, the value from the viewed posting is always returned.
module Penny.Lincoln.Queries.Bundle.Best where

import Penny.Lincoln.TopLine
import Penny.Lincoln.Transaction
import Penny.Lincoln.Pieces
import Penny.Lincoln.Posting
import Penny.Lincoln.Ents
import Penny.Lincoln.HasText
import qualified Data.Text as X

bestTextField
  :: HasText a
  => (PostingData -> a)
  -> (TopLineData -> a)
  -> Bundle
  -> a
bestTextField fp ft b
  | X.null . text $ tp = tt
  | otherwise = tp
  where
    tp = fp . pstgData . entMeta . viewCurrent . bunView $ b
    tt = ft . tlData . bunTopLine $ b

number :: Bundle -> Number
number = bestTextField pstgNumber tlNumber

flag :: Bundle -> Flag
flag = bestTextField pstgFlag tlFlag

payee :: Bundle -> Payee
payee = bestTextField pstgPayee tlPayee

memo :: Bundle -> Memo
memo b = case unMemo pm of
  [] -> tlm
  _ -> pm
  where
    tlm = tlMemo . tlData . bunTopLine $ b
    pm = pstgMemo . pstgData . entMeta . viewCurrent . bunView $ b
