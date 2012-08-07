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
  , I.SubAccountName (SubAccountName, unSubAccountName)
  , I.Account(Account, unAccount)
  
    -- ** Amounts
  , I.Amount (Amount, qty, commodity)
  
    -- ** Commodities
  , I.Commodity (Commodity, unCommodity)
  , I.SubCommodity (SubCommodity, unSubCommodity)
  , I.charCommodity
    
    -- ** DateTime
  , I.DateTime
  , I.dateTime
  , I.localTime
  , I.timeZone
  , I.TimeZoneOffset
  , I.minsToOffset
  , I.offsetToMins
  , I.noOffset
    
    -- ** Debits and credits
  , I.DrCr(Debit, Credit)
  , I.opposite
    
    -- ** Entries
  , I.Entry (Entry, drCr, amount)
    
    -- ** Flag
  , I.Flag (Flag, unFlag)
    
    -- ** Memos
  , I.MemoLine (MemoLine, unMemoLine)
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
  , I.PricePoint(PricePoint, price, ppMeta)

    -- ** Quantities                               
  , I.Qty
  , I.unQty
  , I.partialNewQty
  , I.newQty
  , I.add
  , I.subt
  , I.mult
  , I.zero
  , I.difference
  , I.Difference(LeftBiggerBy, RightBiggerBy, Equal)
    
    -- ** Tags
  , I.Tag(Tag, unTag)
  , I.Tags(Tags, unTags)
    
    
    -- * Builders
  , Bd.crashy
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
    
    -- * HasText
  , HT.HasText(text)
  , HT.Delimited(Delimited)
  , HT.HasTextList(textList)
  , HT.HasTextNonEmpty(textNonEmpty)
  , HT.HasTextNonEmptyList(textNonEmptyList)

    -- * TextNonEmpty
  , TNE.TextNonEmpty(TextNonEmpty)
    
    -- * Transactions
    -- ** Postings and transactions
  , T.Posting
  , T.Transaction
  , T.PostFam

    -- ** Making transactions
  , T.transaction
  , T.Error ( UnbalancedError, CouldNotInferError)
  
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
import qualified Penny.Lincoln.TextNonEmpty as TNE
import qualified Penny.Lincoln.Transaction as T
