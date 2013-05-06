-- | Like 'Penny.Lincoln.Queries' but instead of querying the main
-- posting of the PostFam, queries the siblings. Therefore, these
-- functions return a list, with each entry in the list containing the
-- best answer for each sibling. There is one item in the list for
-- each sibling, even if all these items contain the same data (for
-- instance, a posting might have five siblings, but all five siblings
-- might have the same payee. Nonetheless the 'payee' function will
-- return a list of five items.)
module Penny.Lincoln.Queries.Siblings where

import Control.Arrow (second, first)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Ents as E
import Penny.Lincoln.Balance (Balance, entryToBalance)

-- | For all siblings, uses information from the Posting if it is set;
-- otherwise, uses data from the TopLine.
bestSibs
  :: (B.PostingCore -> Maybe a)
  -> (B.TopLineCore -> Maybe a)
  -> E.ViewedEnt
  -> [Maybe a]
bestSibs fp ft =
  map f
  . map (second (B.pdCore . E.meta))
  . E.unrollSnd
  . second E.tailEnts
  . first B.tlCore
  where
    f (tl, vw) = maybe (ft tl) Just (fp vw)


-- | For all siblings, get the information from the Posting if it
-- exists; otherwise Nothing.
sibs
  :: (E.Ent B.PostingData -> a)
  -> E.ViewedEnt
  -> [a]
sibs fp = map fp . snd . fmap E.tailEnts

payee :: E.ViewedEnt -> [Maybe B.Payee]
payee = bestSibs B.pPayee B.tPayee

number :: E.ViewedEnt -> [Maybe B.Number]
number = bestSibs B.pNumber B.tNumber

flag :: E.ViewedEnt -> [Maybe B.Flag]
flag = bestSibs B.pFlag B.tFlag

postingMemo :: E.ViewedEnt -> [Maybe B.Memo]
postingMemo = sibs (B.pMemo . B.pdCore . E.meta)

account :: E.ViewedEnt -> [B.Account]
account = sibs (B.pAccount . B.pdCore . E.meta)

tags :: E.ViewedEnt -> [B.Tags]
tags = sibs (B.pTags . B.pdCore . E.meta)

entry :: E.ViewedEnt -> [B.Entry]
entry = sibs E.entry

balance :: E.ViewedEnt -> [Balance]
balance = map entryToBalance . entry

drCr :: E.ViewedEnt -> [B.DrCr]
drCr = map B.drCr . entry

amount :: E.ViewedEnt -> [B.Amount]
amount = map B.amount . entry

qty :: E.ViewedEnt -> [B.Qty]
qty = map B.qty . amount

commodity :: E.ViewedEnt -> [B.Commodity]
commodity = map B.commodity . amount

postingLine :: E.ViewedEnt -> [Maybe B.PostingLine]
postingLine = sibs (fmap B.pPostingLine . B.pdFileMeta . E.meta)

side :: E.ViewedEnt -> [Maybe B.Side]
side = sibs (B.pSide . B.pdCore . E.meta)

spaceBetween :: E.ViewedEnt -> [Maybe B.SpaceBetween]
spaceBetween = sibs (B.pSpaceBetween . B.pdCore . E.meta)

filePosting :: E.ViewedEnt -> [Maybe B.FilePosting]
filePosting = sibs (fmap B.pFilePosting . B.pdFileMeta . E.meta)
