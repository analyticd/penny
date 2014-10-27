module Penny.Core.Bundle
  ( T
  , up
  , down
  , toTransaction
  , topLine
  , postings

  -- * Fields
  -- ** Top line fields
  -- | These fields are present only in the 'TopLine.T'.
  , dateTime
  , clxn

  -- ** Best fields

  -- | These fields are present in the 'TopLine.T' and in the
  -- 'Posting.T'; these functions select the field from the
  -- 'Posting.T' if it is set, or the one from the 'TopLine.T'
  -- otherwise.
  , best
  , number
  , flag
  , payee

  -- ** Posting fields

  -- | These fields are present only in postings.
  , ent
  , qty
  , commodity
  , posting
  , tags
  , account
  , trio

  -- ** Both TopLine and Posting

  -- | These fields are in the 'TopLine.T' and the 'Posting.T'; both
  -- are returned.

  , united
  , memo
  , location
  , global
  , local
  ) where

import Penny.Core.Bundle.Internal
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Core.Balanced.Internal as Balanced.Internal
import Data.Sequence ((|>))
import qualified Penny.Core.View as View
import Data.Monoid
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

up :: T -> Maybe T
up (T t v) = fmap (T t) . View.moveLeft $ v

down :: T -> Maybe T
down (T t v) = fmap (T t) . View.moveRight $ v

toTransaction :: T -> Transaction.T
toTransaction (T t (View.T l c r))
  = Transaction.T t (Balanced.Internal.T ((l |> c) <> r))

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
