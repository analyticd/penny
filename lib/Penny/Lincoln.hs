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
  , I.Amount (Amount, qty, commodity)

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
  , I.picoToSeconds
  , I.zeroSeconds
  , I.midnight
  , I.DateTime ( .. )
  , I.toUTC
  , I.toZonedTime
  , I.fromZonedTime
  , I.sameInstant

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
  , I.PricePoint(PricePoint, dateTime, price, ppMeta)

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
  , I.Difference(..)
  , I.allocate

    -- ** Tags
  , I.Tag(Tag, unTag)
  , I.Tags(Tags, unTags)


    -- * Builders
  , Bd.account

    -- * Families
    -- ** Family types
  , F.Family(Family)
  , F.Child(Child)
  , F.Siblings(Siblings)

    -- ** Manipulating families
  , F.children
  , F.orphans
  , F.adopt
  , F.marryWith
  , F.marry
  , F.divorceWith
  , F.divorce
  , F.filterChildren
  , F.find

    -- * HasText
  , HT.HasText(text)
  , HT.HasTextList(textList)

    -- * Transactions
    -- ** Postings and transactions
  , T.Posting
  , T.Transaction
  , T.PostFam

    -- ** Making and deconstructing transactions
  , T.transaction
  , T.Error ( UnbalancedError, CouldNotInferError)
  , T.toUnverified

    -- ** Querying postings
  , T.Inferred(Inferred, NotInferred)
  , T.pPayee
  , T.pNumber
  , T.pFlag
  , T.pAccount
  , T.pTags
  , T.pEntry
  , T.pMemo
  , T.pInferred
  , T.pMeta
  , T.changePostingMeta

    -- ** Querying transactions
  , T.TopLine
  , T.tDateTime
  , T.tFlag
  , T.tNumber
  , T.tPayee
  , T.tMemo
  , T.tMeta
  , T.changeTransactionMeta
  , T.postFam

    -- ** Adding serials to transactions
  , T.addSerialsToList
  , T.addSerialsToEithers

    -- ** Unwrapping Transactions
  , T.unTransaction
  , T.unPostFam

    -- ** Transaction boxes
  , T.Box (Box, boxMeta, boxPostFam)

  -- * Metadata
  , M.TopLineLine(TopLineLine, unTopLineLine)
  , M.TopMemoLine(TopMemoLine, unTopMemoLine)
  , M.Side(CommodityOnLeft, CommodityOnRight)
  , M.SpaceBetween(SpaceBetween, NoSpaceBetween)
  , M.Format(Format, side, between)
  , M.Filename(Filename, unFilename)
  , M.PriceLine(PriceLine, unPriceLine)
  , M.PostingLine(PostingLine, unPostingLine)
  , M.PriceMeta(PriceMeta, priceLine, priceFormat)
  , M.GlobalPosting(GlobalPosting, unGlobalPosting)
  , M.FilePosting(FilePosting, unFilePosting)
  , M.GlobalTransaction(GlobalTransaction, unGlobalTransaction)
  , M.FileTransaction(FileTransaction, unFileTransaction)
  , M.PostingMeta(PostingMeta, postingLine, postingFormat,
                  globalPosting, filePosting)
  , M.emptyPostingMeta
  , M.TopLineMeta(TopLineMeta, topMemoLine, topLineLine, filename,
                globalTransaction, fileTransaction)
  , M.emptyTopLineMeta

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
  , S.serials
  , S.serialItems

    -- * Matchers
  , Matchers.Matcher
  , Matchers.Factory

  ) where

import qualified Penny.Lincoln.Balance as B
import qualified Penny.Lincoln.Bits as I
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Lincoln.Family as F
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.Matchers as Matchers
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.PriceDb as DB
import qualified Penny.Lincoln.Serial as S
import qualified Penny.Lincoln.Transaction as T
