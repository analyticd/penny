-- | Essential data types used to make Transactions and Postings.
module Penny.Lincoln.Bits (
  -- * Accounts
  O.SubAccount(SubAccount, unSubAccount),
  O.Account(Account, unAccount),

  -- * Amounts
  O.Amount(Amount, qty, commodity),

  -- * Commodities
  O.Commodity(Commodity, unCommodity),

  -- * DateTime
  DT.TimeZoneOffset ( offsetToMins ),
  DT.minsToOffset,
  DT.noOffset,
  DT.Hours ( unHours ),
  DT.intToHours,
  DT.zeroHours,
  DT.Minutes ( unMinutes ),
  DT.intToMinutes,
  DT.zeroMinutes,
  DT.midnight,
  DT.Seconds ( unSeconds ),
  DT.intToSeconds,
  DT.zeroSeconds,
  DT.DateTime ( .. ),
  DT.dateTimeMidnightUTC,
  DT.toUTC,
  DT.toZonedTime,
  DT.fromZonedTime,
  DT.sameInstant,

  -- * Debits and Credits
  O.DrCr(Debit, Credit),
  O.opposite,

  -- * Entries
  O.Entry(Entry, drCr, amount),

  -- * Flag
  O.Flag(Flag, unFlag),

  -- * Memos
  O.Memo(Memo, unMemo),

  -- * Number
  O.Number(Number, unNumber),

  -- * Payee
  O.Payee(Payee, unPayee),

  -- * Prices and price points
  Pr.From(From, unFrom), Pr.To(To, unTo),
  Pr.CountPerUnit(CountPerUnit, unCountPerUnit),
  Pr.Price(from, to, countPerUnit),
  Pr.convert, Pr.newPrice,
  PricePoint ( .. ),

  -- * Quantities
  Q.Qty, Q.NumberStr(..), Q.toQty, Q.mantissa, Q.places,
  Q.add, Q.mult, Q.difference, Q.equivalent, Q.newQty,
  Q.Mantissa, Q.Places,
  Q.Difference(Q.LeftBiggerBy, Q.RightBiggerBy, Q.Equal),
  Q.allocate,

  -- * Tags
  O.Tag(Tag, unTag),
  O.Tags(Tags, unTags),

  -- * Metadata
  O.TopLineLine(..),
  O.TopMemoLine(..),
  O.Side(..),
  O.SpaceBetween(..),
  O.Filename(..),
  O.PriceLine(..),
  O.PostingLine(..),
  O.GlobalPosting(..),
  O.FilePosting(..),
  O.GlobalTransaction(..),
  O.FileTransaction(..)
  ) where


import qualified Penny.Lincoln.Bits.Open as O
import qualified Penny.Lincoln.Bits.DateTime as DT
import qualified Penny.Lincoln.Bits.Price as Pr
import qualified Penny.Lincoln.Bits.Qty as Q
import qualified Penny.Lincoln.Meta as M

data PricePoint = PricePoint { dateTime :: DT.DateTime
                             , price :: Pr.Price
                             , ppMeta :: M.PriceMeta }
                  deriving (Eq, Show)
