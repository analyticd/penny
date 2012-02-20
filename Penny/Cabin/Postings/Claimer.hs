module Penny.Cabin.Postings.Claimer where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Array as A
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as X

import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Meta as Me
import Penny.Lincoln.HasText (text)
import qualified Penny.Lincoln.HasText as HT

import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

ifShown ::
  (F.Fields Bool -> Bool)
  -> F.Fields Bool
  -> Maybe T.ClaimedWidth
  -> Maybe T.ClaimedWidth
ifShown fn flds mt =
  if fn flds
  then mt
  else Nothing
 
type Claimer = 
  F.Fields Bool
  -> O.Options
  -> A.Array (Col, (T.VisibleNum, Row)) T.PostingInfo
  -> (Col, (T.VisibleNum, Row))
  -> T.PostingInfo
  -> Maybe T.ClaimedWidth

claimer :: Claimer
claimer flds opts a (col, (vn, r)) p = let
  f = claimLookup ! (col, r) in
  f flds opts a (col, (vn, r)) p

claimLookup :: M.Map (Col, Row) Claimer
claimLookup = foldl (flip . uncurry $ M.insert) noClaims ls where
  noClaims = M.fromList
    $ (,)
    <$> A.range ((minBound, minBound), (maxBound, maxBound))
    <*> pure noClaim
  ls = [lineNum, sLineNum,
        date, sDate]

noClaim :: Claimer
noClaim _ _ _ _ _ = Nothing

claimOne :: Maybe T.ClaimedWidth
claimOne = Just $ T.ClaimedWidth 1

claimOneIf :: Bool -> Maybe T.ClaimedWidth
claimOneIf b =
  if b
  then claimOne
  else Nothing

lineNum :: ((Col, Row), Claimer)
lineNum = ((Adr.LineNum, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.lineNum flds $
    case Q.postingLine . T.postingBox $ p of
      Nothing -> Nothing
      (Just n) ->
        Just
        . T.ClaimedWidth
        . length
        . show
        . Me.unLine
        . Me.unPostingLine
        $ n

sLineNum :: ((Col, Row), Claimer)
sLineNum = ((Adr.SLineNum, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.lineNum flds
    . claimOneIf
    . isJust
    . Q.postingLine
    . T.postingBox
    $ p

date :: ((Col, Row), Claimer)
date = ((Adr.Date, Adr.Top), f) where
  f flds opts _ _ p =
    ifShown F.date flds $
    Just
    . T.ClaimedWidth
    . X.length
    . O.dateFormat opts
    $ p

sDate :: ((Col, Row), Claimer)
sDate = ((Adr.SDate, Adr.Top), f) where
  f flds _ _ _ _ = ifShown F.date flds claimOne


flag :: ((Col, Row), Claimer)
flag = ((Adr.Multi, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.flag flds $
    case Q.flag . T.postingBox $ p of
      Nothing -> Nothing
      (Just flag) ->
        Just
        . T.ClaimedWidth
        . X.length
        . text
        $ flag

sFlag :: ((Col, Row), Claimer)
sFlag = ((Adr.SMulti, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.flag flds
    . claimOneIf
    . isJust
    . Q.flag
    . T.postingBox
    $ p

number :: ((Col, Row), Claimer)
number = ((Adr.Num, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.number flds $
    case Q.number . T.postingBox $ p of
      Nothing -> Nothing
      (Just num) ->
        Just
        . T.ClaimedWidth
        . X.length
        . text
        $ num

sNumber :: ((Col, Row), Claimer)
sNumber = ((Adr.SNum, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.flag flds
    . claimOneIf
    . isJust
    . Q.number
    . T.postingBox
    $ p

payee :: ((Col, Row), Claimer)
payee = ((Adr.Payee, Adr.Top), noClaim)

sPayee :: ((Col, Row), Claimer)
sPayee = ((Adr.SPayee, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.payee flds
    . claimOneIf
    . isJust
    . Q.payee
    . T.postingBox
    $ p

account :: ((Col, Row), Claimer)
account = ((Adr.Account, Adr.Top), noClaim)

sAccount :: ((Col, Row), Claimer)
sAccount = ((Adr.SAccount, Adr.Top), f) where
  f flds _ _ _ p = ifShown F.account flds claimOne

postingDrCr :: ((Col, Row), Claimer)
postingDrCr = ((Adr.PostingDrCr, Adr.Top), f) where
  f flds _ _ _ p = ifShown F.postingDrCr flds $
                   Just (T.ClaimedWidth 2)

sPostingDrCr :: ((Col, Row), Claimer)
sPostingDrCr = ((Adr.SPostingDrCr, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.postingDrCr flds claimOne

postingCmdty :: ((Col, Row), Claimer)
postingCmdty = ((Adr.PostingCommodity, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.postingCmdty flds $
    Just
    . T.ClaimedWidth
    . X.length
    . text
    . HT.Delimited (X.singleton ':')
    . HT.textList
    . Q.commodity
    . T.postingBox
    $ p

sPostingCmdty :: ((Col, Row), Claimer)
sPostingCmdty = ((Adr.SPostingCommodity, Adr.Top), f) where
  f flds _ _ _ p = ifShown F.postingCmdty flds claimOne

--postingDrCr :: ((Col, Row), Claimer)
--postingDrCr = ((Adr.Posting
