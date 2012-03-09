module Penny.Lincoln.Meta where

import qualified Penny.Lincoln.Family.Family as F

import qualified Data.Text as X

data Side = CommodityOnLeft | CommodityOnRight deriving Show
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving Show

data Format =
  Format { side :: Side
         , between :: SpaceBetween }
  deriving Show

newtype Line = Line { unLine :: Int }
               deriving Show

newtype Filename = Filename { unFilename :: X.Text }
                   deriving Show

newtype Column = Column { unColumn :: Int }
                 deriving (Show, Eq, Ord)

newtype PriceLine = PriceLine { unPriceLine :: Line }
                    deriving Show

newtype PostingLine = PostingLine { unPostingLine :: Line }
                      deriving Show

newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Line }
                      deriving Show

newtype TopLineLine = TopLineLine { unTopLineLine :: Line }
                      deriving Show

data PriceMeta =
  PriceMeta { priceLine :: PriceLine
            , priceFormat :: Format }
  deriving Show

data PostingMeta =
  PostingMeta { postingLine :: Maybe PostingLine
              , postingFormat :: Maybe Format }
  deriving Show

data TopLineMeta =
  TopLineMeta { topMemoLine :: Maybe TopMemoLine
              , topLineLine :: Maybe TopLineLine
              , filename :: Maybe Filename }
  deriving Show

newtype TransactionMeta =
  TransactionMeta
  { unTransactionMeta :: F.Family TopLineMeta PostingMeta }
  deriving Show
