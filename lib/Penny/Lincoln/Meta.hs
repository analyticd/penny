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

newtype PriceLine = PriceLine { unPriceLine :: Line }
                    deriving (Eq, Show)

newtype PostingLine = PostingLine { unPostingLine :: Line }
                      deriving (Eq, Show)

newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Line }
                      deriving (Eq, Show)

newtype TopLineLine = TopLineLine { unTopLineLine :: Line }
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
