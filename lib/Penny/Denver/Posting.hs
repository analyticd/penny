module Penny.Denver.Posting where

import qualified Penny.Denver.Common as C
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.TextNonEmpty as TNE

data Record = Record {
  entry :: Entry
  , price :: Maybe Price
  } deriving (Eq, Show)

data Posting = Posting {
  cleared :: C.Cleared
  , account :: B.Account
  , record :: Maybe Record
  , memo :: Maybe B.MemoLine
  } deriving (Eq, Show)

data Entry = Entry {
  sign :: Sign
  , amount :: Amount
  , entryFormat :: M.Format
  } deriving (Eq, Show)

data Sign = Positive | Negative
          deriving (Eq, Show)

data Amount = Amount {
  qty :: B.Qty
  , commodity :: Commodity
  } deriving (Eq, Show)

newtype Commodity = Commodity {
  unCommodity :: TNE.TextNonEmpty
  } deriving (Eq, Show)

-- | All prices are unit prices; my file does not contain any total
-- prices (the kind indicated with two at signs).
data Price = Price {
  toCommodity :: Commodity
  , qtyPerUnit :: B.Qty
  , priceFormat :: M.Format
  } deriving (Eq, Show)
