-- | Like 'Penny.Lincoln.Queries' but instead of querying the main
-- posting of the PostFam, queries the siblings. Therefore, these
-- functions return a list, with each entry in the list containing the
-- best answer for each sibling. There is one item in the list for
-- each sibling, even if all these items contain the same data (for
-- instance, a posting might have five siblings, but all five siblings
-- might have the same payee. Nonetheless the 'payee' function will
-- return a list of five items.)
module Penny.Lincoln.Queries.Siblings where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family.Child (Child(Child))
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)

-- | For all siblings, uses information from the Posting if it is set;
-- otherwise, uses data from the TopLine.
bestSibs
  :: (T.Posting -> Maybe a)
  -> (T.TopLine -> Maybe a)
  -> T.PostFam
  -> [Maybe a]
bestSibs fp ft pf =
  let (Child _ s1 ss p) = T.unPostFam pf
      get = maybe (ft p) Just . fp
  in get s1 : map get ss

-- | For all siblings, get the information from the Posting if it
-- exists; otherwise Nothing.
sibs
  :: (T.Posting -> a)
  -> T.PostFam
  -> [a]
sibs fp pf =
  let (Child _ s1 ss _) = T.unPostFam pf
  in fp s1 : map fp ss

payee :: T.PostFam -> [Maybe B.Payee]
payee = bestSibs T.pPayee T.tPayee

number :: T.PostFam -> [Maybe B.Number]
number = bestSibs T.pNumber T.tNumber

flag :: T.PostFam -> [Maybe B.Flag]
flag = bestSibs T.pFlag T.tFlag

postingMemo :: T.PostFam -> [Maybe B.Memo]
postingMemo = sibs T.pMemo

account :: T.PostFam -> [B.Account]
account = sibs T.pAccount

tags :: T.PostFam -> [B.Tags]
tags = sibs T.pTags

entry :: T.PostFam -> [B.Entry]
entry = sibs T.pEntry

balance :: T.PostFam -> [Balance]
balance = map entryToBalance . entry

drCr :: T.PostFam -> [B.DrCr]
drCr = map B.drCr . entry

amount :: T.PostFam -> [B.Amount]
amount = map B.amount . entry

qty :: T.PostFam -> [B.Qty]
qty = map B.qty . amount

commodity :: T.PostFam -> [B.Commodity]
commodity = map B.commodity . amount

postingLine :: T.PostFam -> [Maybe B.PostingLine]
postingLine = sibs T.pPostingLine

side :: T.PostFam -> [Maybe B.Side]
side = map B.side . amount

spaceBetween :: T.PostFam -> [Maybe B.SpaceBetween]
spaceBetween = map B.spaceBetween . amount

globalPosting :: T.PostFam -> [Maybe B.GlobalPosting]
globalPosting = sibs T.pGlobalPosting

filePosting :: T.PostFam -> [Maybe B.FilePosting]
filePosting = sibs T.pFilePosting
