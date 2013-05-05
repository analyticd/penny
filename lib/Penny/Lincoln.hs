-- | Lincoln - the Penny core
--
-- Penny's core types and classes are here. This module re-exports the
-- most useful things. For more details you will want to look at the
-- sub-modules. Also, not all types and functions are re-exported due
-- to naming conflicts. In particular, neither
-- "Penny.Lincoln.Predicates" nor "Penny.Lincoln.Queries" is exported
-- from here due to the blizzard of name conflicts that would result.
module Penny.Lincoln (
  -- * Balances
  B.Balance
  , B.unBalance
  , B.Balanced(Balanced, Inferable, NotInferable)
  , B.isBalanced
  , B.entryToBalance
  , B.addBalances
  , B.removeZeroCommodities
  , B.BottomLine (Zero, NonZero)
  , B.Column (Column)

    -- * Bits
    -- ** Accounts
  , I.SubAccount (SubAccount, unSubAccount)
  , I.Account(Account, unAccount)

    -- ** Amounts
  , I.Amount (Amount, qty, commodity, side, spaceBetween)

    -- ** Commodities
  , I.Commodity (Commodity, unCommodity)

    -- ** DateTime
  , I.TimeZoneOffset ( offsetToMins )
  , I.minsToOffset
  , I.noOffset
  , I.Hours ( unHours )
  , I.intToHours
  , I.Minutes ( unMinutes )
  , I.intToMinutes
  , I.Seconds ( unSeconds )
  , I.intToSeconds
  , I.zeroSeconds
  , I.midnight
  , I.DateTime ( .. )
  , I.dateTimeMidnightUTC
  , I.toUTC
  , I.toZonedTime
  , I.fromZonedTime
  , I.sameInstant
  , I.showDateTime

    -- ** Debits and credits
  , I.DrCr(Debit, Credit)
  , I.opposite

    -- ** Entries
  , I.Entry (Entry, drCr, amount)

    -- ** Flag
  , I.Flag (Flag, unFlag)

    -- ** Memos
  , I.Memo (Memo, unMemo)

    -- ** Number
  , I.Number (Number, unNumber)

    -- ** Payee
  , I.Payee (Payee, unPayee)

    -- ** Prices and price points
  , I.From(From, unFrom)
  , I.To(To, unTo)
  , I.CountPerUnit(CountPerUnit, unCountPerUnit)
  , I.Price(from, to, countPerUnit)
  , I.newPrice
  , I.PricePoint ( PricePoint, dateTime, price, ppSide,
                   ppSpaceBetween, priceLine)

    -- ** Quantities
  , I.Qty
  , I.NumberStr(..)
  , I.toQty
  , I.mantissa
  , I.places
  , I.add
  , I.mult
  , I.difference
  , I.equivalent
  , I.newQty
  , I.Mantissa, I.Places
  , I.Difference(..)
  , I.allocate

    -- ** Tags
  , I.Tag(Tag, unTag)
  , I.Tags(Tags, unTags)


    -- * Builders
  , Bd.account

    -- * HasText
  , HT.HasText(text)
  , HT.HasTextList(textList)

    -- * Transactions
    -- ** Postings and transactions
  , T.Posting
  , T.Inferred(..)
  , T.pEntry
  , T.pInferred
  , T.pMeta
  , T.Transaction
  , T.unTransaction
  , T.mapPostings
  , T.transaction
  , T.rTransaction
  , T.View
  , T.unView
  , T.views

  -- * Metadata
  , I.TopLineLine(..)
  , I.TopMemoLine(..)
  , I.Side(CommodityOnLeft, CommodityOnRight)
  , I.SpaceBetween(SpaceBetween, NoSpaceBetween)
  , I.Filename(Filename, unFilename)
  , I.PriceLine(PriceLine, unPriceLine)
  , I.PostingLine(PostingLine, unPostingLine)
  , I.GlobalPosting(GlobalPosting, unGlobalPosting)
  , I.FilePosting(FilePosting, unFilePosting)
  , I.GlobalTransaction(GlobalTransaction, unGlobalTransaction)
  , I.FileTransaction(FileTransaction, unFileTransaction)

    -- * PriceDb
  , DB.PriceDb
  , DB.emptyDb
  , DB.addPrice
  , DB.getPrice
  , DB.PriceDbError(FromNotFound, ToNotFound, CpuNotFound)
  , DB.convert

    -- * Serials
  , S.Serial
  , S.forward
  , S.backward
  , S.GenSerial
  , S.incrementBack
  , S.getSerial
  , S.makeSerials
  , S.serialItems
  , S.nSerials

    -- * Matchers
  , Matchers.Factory

    -- * Showing postFam in one line
  , display

  ) where

import qualified Penny.Lincoln.Balance as B
import qualified Penny.Lincoln.Bits as I
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Lincoln.Family as F
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.Matchers as Matchers
import qualified Penny.Lincoln.PriceDb as DB
import qualified Penny.Lincoln.Serial as S
import qualified Penny.Lincoln.Transaction as T

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Penny.Lincoln.Queries as Q
import qualified Data.Time as Time
import System.Locale (defaultTimeLocale)

--
-- Display
--

-- | Displays a PostFam in a one line format.
--
-- Format:
--
-- File LineNo Date Payee Acct DrCr Cmdty Qty
display :: T.ViewedPosting -> Text
display p = X.pack $ concat (intersperse " " ls)
  where
    ls = [file, lineNo, dt, pye, acct, dc, cmdty, qt]
    file = maybe (labelNo "filename") (X.unpack . I.unFilename)
           (fmap I.tFilename . tlFileMeta . fst $ p)
    lineNo = maybe (labelNo "line number")
             (show . I.unPostingLine)
             (fmap I.pPostingLine . I.pdFileMeta
                   . T.pMeta . T.headPosting
                   . snd $ p)
    dateFormat = "%Y-%m-%d %T %z"
    dt = Time.formatTime defaultTimeLocale dateFormat
         . Time.utctDay
         . I.toUTC
         . 
         . Q.dateTime
         $ p
    pye = maybe (labelNo "payee")
            (X.unpack . HT.text) (Q.payee p)
    acct = X.unpack . X.intercalate (X.singleton ':')
           . map I.unSubAccount . I.unAccount . Q.account $ p
    dc = case Q.drCr p of
      I.Debit -> "Dr"
      I.Credit -> "Cr"
    cmdty = X.unpack . I.unCommodity . Q.commodity $ p
    qt = show . Q.qty $ p
    labelNo s = "(no " ++ s ++ ")"
