module Penny.Denver.Transaction where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as X
import qualified Penny.Denver.Common as C
import qualified Penny.Lincoln.Bits as B
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
                    , meta :: M.Format }
  deriving (Eq, Show)

lincolnizePstg ::
  P.Posting
  -> NE.NonEmpty PostingWithFormat
lincolnizePstg p = undefined

lincolnizeCleared :: C.Cleared -> Maybe B.Flag
lincolnizeCleared c = case c of
  C.Cleared -> Just (B.Flag (TNE.TextNonEmpty '*' X.empty))
  C.NotCleared -> Nothing

lincolnizeValued ::
  C.Cleared
  -> B.Account
  -> P.Entry
  -> P.Price
  -> Maybe B.MemoLine
  -> (PostingWithFormat, PostingWithFormat, PostingWithFormat)
lincolnizeValued = undefined

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
