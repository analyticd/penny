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
  :: (T.Posting pm -> Maybe a)
  -> (T.TopLine tm -> Maybe a)
  -> T.PostFam tm pm
  -> [Maybe a]
bestSibs fp ft pf =
  let (Child _ s1 ss p) = T.unPostFam pf
      get = maybe (ft p) Just . fp
  in get s1 : map get ss

-- | For all siblings, get the information from the Posting if it
-- exists; otherwise Nothing.
sibs
  :: (T.Posting pm -> a)
  -> T.PostFam tm pm
  -> [a]
sibs fp pf =
  let (Child _ s1 ss _) = T.unPostFam pf
  in fp s1 : map fp ss

payee :: T.PostFam tm pm -> [Maybe B.Payee]
payee = bestSibs T.pPayee T.tPayee

number :: T.PostFam tm pm -> [Maybe B.Number]
number = bestSibs T.pNumber T.tNumber

flag :: T.PostFam tm pm -> [Maybe B.Flag]
flag = bestSibs T.pFlag T.tFlag

postingMemo :: T.PostFam tm pm -> [Maybe B.Memo]
postingMemo = sibs T.pMemo

account :: T.PostFam tm pm -> [B.Account]
account = sibs T.pAccount

tags :: T.PostFam tm pm -> [B.Tags]
tags = sibs T.pTags

entry :: T.PostFam tm pm -> [B.Entry]
entry = sibs T.pEntry

balance :: T.PostFam tm pm -> [Balance]
balance = map entryToBalance . entry

drCr :: T.PostFam tm pm -> [B.DrCr]
drCr = map B.drCr . entry

amount :: T.PostFam tm pm -> [B.Amount]
amount = map B.amount . entry

qty :: T.PostFam tm pm -> [B.Qty]
qty = map B.qty . amount

commodity :: T.PostFam tm pm -> [B.Commodity]
commodity = map B.commodity . amount

postingLine :: T.PostFam tm pm -> [Maybe B.PostingLine]
postingLine = sibs T.pPostingLine

side :: T.PostFam tm pm -> [Maybe B.Side]
side = map B.side . amount

spaceBetween :: T.PostFam tm pm -> [Maybe B.SpaceBetween]
spaceBetween = map B.spaceBetween . amount

filePosting :: T.PostFam tm pm -> [Maybe B.FilePosting]
filePosting = sibs T.pFilePosting
