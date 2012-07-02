-- | Transaction and posting metadata. Also useful here are the types
-- and function in "Penny.Lincoln.Boxes", which combine metadata with
-- postings and prices.
module Penny.Lincoln.Meta where

import qualified Penny.Lincoln.Family.Family as F

import qualified Data.Text as X

data Side = CommodityOnLeft | CommodityOnRight deriving (Eq, Show)
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving (Eq, Show)

data Format =
  Format { side :: Side
         , between :: SpaceBetween }
  deriving (Eq, Show)

newtype Line = Line { unLine :: Int }
               deriving (Eq, Show)

newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

newtype Column = Column { unColumn :: Int }
                 deriving (Show, Eq, Ord)

-- | The line that a price appears on inside of a file.
newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show)

-- | The line that a posting appears on inside of a file.
newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show)

-- | The line on which the memo of a transaction begins.
newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show)

-- | The line on which the top line of a transaction is.
newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show)

-- | Transactions are numbered in sequence, beginning at zero, from
-- first to last.
newtype TransactionNum =
  TransactionNum { unTransactionNum :: Int }
  deriving (Eq, Show)

-- | Transactions numbered in sequence, beginning at zero, from last
-- to first.
newtype RevTransactionNum =
  RevTransactionNum { unRevTransactionNum :: Int }
  deriving (Eq, Show)

-- | Postings are numbered in sequence, beginning at zero, from first
-- to last. Numbered after transactions are split into postings, but
-- before postings are filtered.
newtype UnfilteredNum =
  UnfilteredNum { unUnfilteredNum :: Int }
  deriving (Eq, Show)

-- | Unfiltered transactions, numbered from last to first begining at
-- zero.
newtype RevUnfilteredNum =
  RevUnfilteredNum { unRevUnfilteredNum :: Int }
  deriving (Eq, Show)

-- | Postings are numbered in sequence, beginning at zero, from first
-- to last, after the postings have been filtered.
newtype FilteredNum =
  FilteredNum { unFilteredNum :: Int }
  deriving (Eq, Show)



data PriceMeta =
  PriceMeta { priceLine :: Maybe PriceLine
            , priceFormat :: Maybe Format }
  deriving (Eq, Show)

data PostingMeta =
  PostingMeta { postingLine :: Maybe PostingLine
              , postingFormat :: Maybe Format }
  deriving (Eq, Show)

data TopLineMeta =
  TopLineMeta { topMemoLine :: Maybe TopMemoLine
              , topLineLine :: Maybe TopLineLine
              , filename :: Maybe Filename }
  deriving (Eq, Show)

newtype TransactionMeta =
  TransactionMeta
  { unTransactionMeta :: F.Family TopLineMeta PostingMeta }
  deriving (Eq, Show)
