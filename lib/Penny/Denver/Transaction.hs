module Penny.Denver.Transaction where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Denver.Common as C
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.TextNonEmpty as TNE
import qualified Penny.Denver.Posting as P
import qualified Penny.Denver.TopLine as TL
import qualified Penny.Lincoln.Transaction.Unverified as U

newtype Transaction =
  Transaction { unTransaction :: F.Family TL.TopLine P.Posting }
  deriving (Eq, Show)

data PostingWithFormat =
  PostingWithFormat { posting :: U.Posting
                    , postingFmt :: M.Format }
  deriving (Eq, Show)

data PriceWithFormat =
  PriceWithFormat {
    price :: B.PricePoint
    , priceFmt :: M.Format
    } deriving (Eq, Show)

lincolnizePstg ::
  P.Posting
  -> NE.NonEmpty PostingWithFormat
lincolnizePstg p = undefined

lincolnizeCleared :: C.Cleared -> Maybe B.Flag
lincolnizeCleared c = case c of
  C.Cleared -> Just (B.Flag (TNE.TextNonEmpty '*' X.empty))
  C.NotCleared -> Nothing

data Valued = Valued {
  original :: PostingWithFormat
  , offset :: PostingWithFormat
  , priced :: PostingWithFormat
  , vPrice :: PriceWithFormat
  } deriving (Eq, Show)

lincolnizeValued ::
  T.Day
  -> C.Cleared
  -> B.Account
  -> P.Entry
  -> P.Price
  -> Maybe B.MemoLine
  -> Valued
lincolnizeValued = undefined

lincolnizeOriginal ::
  C.Cleared
  -> B.Account
  -> P.Entry
  -> Maybe B.MemoLine
  -> PostingWithFormat
lincolnizeOriginal c a e mayMemo = PostingWithFormat p fmt where
  fmt = P.entryFormat e
  p = U.Posting Nothing Nothing fl a (B.Tags []) (Just e') memo
  fl = lincolnizeCleared c
  memo = lincolnizeMemo mayMemo
  e' = B.Entry (lincolnizeSign (P.sign e)) amt
  amt = lincolnizeAmount (P.amount e)

lincolnizeOffset :: P.Entry -> PostingWithFormat
lincolnizeOffset e = PostingWithFormat p fmt where
  fmt = P.entryFormat e
  p = U.Posting Nothing Nothing Nothing tradingAcct (B.Tags [])
      (Just e') (B.Memo [])
  e' = B.Entry dc amt
  dc = B.opposite (lincolnizeSign (P.sign e))
  amt = lincolnizeAmount (P.amount e)

lincolnizePriced :: P.Entry -> P.Price -> PostingWithFormat
lincolnizePriced e pr = PostingWithFormat p fmt where
  fmt = P.priceFormat pr
  p = U.Posting Nothing Nothing Nothing tradingAcct (B.Tags [])
      (Just e') (B.Memo [])
  e' = B.Entry dc amt
  dc = lincolnizeSign (P.sign e)
  amt = B.Amount qty (lincolnizeCommodity (P.toCommodity pr))
  qty = B.mult (P.qtyPerUnit pr) (P.qty . P.amount $ e)
  
makePrice :: T.Day -> P.Entry -> P.Price -> PriceWithFormat
makePrice d e pr = PriceWithFormat pp fmt where
  fmt = P.priceFormat pr
  pp = B.PricePoint dt pr'
  

tradingAcct :: B.Account
tradingAcct = Bd.crashy . Bd.account $ "Income:Trading"

lincolnizeMemo :: Maybe B.MemoLine -> B.Memo
lincolnizeMemo mayMemo = case mayMemo of
  Nothing -> B.Memo []
  Just ml -> B.Memo [ml]

lincolnizeSign :: P.Sign -> B.DrCr
lincolnizeSign s = case s of
  P.Positive -> B.Debit
  P.Negative -> B.Credit

lincolnizeCommodity :: P.Commodity -> B.Commodity
lincolnizeCommodity (P.Commodity tne) = B.Commodity (s1:|[]) where
  s1 = B.SubCommodity tne

lincolnizeAmount :: P.Amount -> B.Amount
lincolnizeAmount (P.Amount q c) = B.Amount q (lincolnizeCommodity c)

{-
Values

Take this transaction for example:

2011/03/02 Broker
    Assets:Brokerage             100 F @ $5
    Assets:Checking              $-500.00

The value of the first posting is $500.

This translates to Penny like so:

@ 2011/03/02 F $5

2011/03/02 Broker
    Assets:Brokerage             Dr 100 F
    Income:Trading               Cr 100 F
    Income:Trading               Dr $500
    Assets:Checking              Cr $500.00

A posting with a price generates 3 postings:

1. A posting whose DrCr is determined by the sign on the Ledger
entry. Its commodity is the same as the commodity on the Ledger
entry. Its quantity is the same as the quantity on the Ledger entry.
Its account is the same as the account on the Ledger entry.

2. A posting whose account is Income:Trading. Its DrCr is the opposite
of the DrCr in number 1. Its commodity and quantity are the same as
in number 1.

3. A posting whose account is Income:Trading. Its DrCr is the same as
in number 1. Its commodity is the same as the commodity on the
Ledger price. Its quantity is the quantity in the Ledger price
multiplied by the quantity on the Ledger entry.

You also generate a price. Its From commodity is the same
as the commodity on the Ledger entry. Its To commodity is
the commodity on the Ledger price. The countPerUnit is the same
as the quantity on the Ledger price.

Another example. This Penny transaction and price:

@ 2011/06/06 LUV $10

2011/06/06 Sell stock
    Assets:Brokerage                 Cr 100 LUV
    Income:Trading                   Dr 100 LUV
    Income:Trading                   Cr $1000
    Assets:Checking                  Dr $1000

translates to this Ledger transaction:
2011/06/06 Sell stock
    Assets:Brokerage                -100 LUV @ $10
    Assets:Checking                 $1000
-}
