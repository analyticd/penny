-- after Ida B. Wells
module Penny.Core.Wells where

import qualified Penny.Core.Serial as Serial
import qualified Penny.Core.Fortune as Fortune
import qualified Penny.Core.Ent as Ent
import qualified Penny.Core.Qty as Qty
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Posting as Posting
import qualified Penny.Core.DateTime as DateTime
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Number as Number
import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.Tags as Tags
import qualified Penny.Core.Account as Account
import qualified Penny.Core.Trio as Trio
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Core.United as United
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Data.Foldable as F
import qualified Penny.Core.Bundle as Bundle
import qualified Penny.Core.View as View

-- | A 'Penny.Core.Fortune.T', combined with some additional serials
-- and a running balance.  This type is used only for postings that
-- are visible in the report.

data T = T
  { fortune :: Fortune.T

  , postReportFilter :: Serial.T
  -- ^ After postings are filtered to determine which postings are in
  -- the report but are not visible, the visible postings are assigned
  -- this serial.  Postings that are not visible still count toward
  -- the report's running balance.
  } deriving (Eq, Ord, Show)

topLine :: T -> TopLine.T
topLine = Fortune.topLine . fortune

-- | Create a list of 'T' from a list of 'Fortune.T'.  Presumably the
-- list of 'Fortune.T' has already been filtered.  The input list must
-- be finite.

fromFortuneList :: [Fortune.T] -> [T]
fromFortuneList fs =
  zipWith T fs (F.toList $ Serial.serials (length fs))

postings :: T -> View.T Posting.T
postings = Bundle.postings . Fortune.bundle . fortune

ent :: T -> Ent.T Posting.T
ent = View.current . postings

posting :: T -> Posting.T
posting = Ent.meta . ent

dateTime :: T -> DateTime.T
dateTime = TopLine.dateTime . topLine

clxn :: T -> Clxn.T
clxn = TopLine.clxn . topLine

best
  :: (TopLine.T -> Maybe a)
  -> (Posting.T -> Maybe a)
  -> T
  -> Maybe a
best ft fp t = maybe (ft . topLine $ t)
  Just (fp . posting $ t)

number :: T -> Maybe Number.T
number = best TopLine.number Posting.number

flag :: T -> Maybe Flag.T
flag = best TopLine.flag Posting.flag

payee :: T -> Maybe Payee.T
payee = best TopLine.payee Posting.payee

qty :: T -> Qty.T
qty = Ent.qty . ent

commodity :: T -> Commodity.T
commodity = Ent.commodity . ent

tags :: T -> Tags.T
tags = Posting.tags . posting

account :: T -> Account.T
account = Posting.account . posting

trio :: T -> Trio.T
trio = Posting.trio . posting

united
  :: (TopLine.T -> a)
  -> (Posting.T -> a)
  -> T
  -> United.T a
united ft fp t = United.T (ft . topLine $ t)
  (fp . posting $ t)

memo :: T -> United.T Memo.T
memo = united TopLine.memo Posting.memo

location :: T -> United.T Location.T
location = united TopLine.location Posting.location

global :: T -> United.T Global.T
global = united TopLine.global Posting.global

local :: T -> United.T Local.T
local = united TopLine.local Posting.local
