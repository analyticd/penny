module Penny.Copper.Meta where

import qualified Data.Text as X

newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show)

newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show)

data Side = CommodityOnLeft | CommodityOnRight deriving (Eq, Show)
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving (Eq, Show)

data Format =
  Format { side :: Side
         , between :: SpaceBetween }
  deriving (Eq, Show)

newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

newtype Column = Column { unColumn :: Int }
                 deriving (Show, Eq, Ord)

newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show)

newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show)

data PriceMeta =
  PriceMeta { priceLine :: PriceLine
            , priceFormat :: Format }
  deriving (Eq, Show)

data PostingMeta =
  PostingMeta { postingLine :: PostingLine
              , postingFormat :: Maybe Format }
  deriving (Eq, Show)

data TopLineMeta =
  TopLineMeta { topMemoLine :: TopMemoLine
              , topLineLine :: TopLineLine
              , filename :: Filename }
  deriving (Eq, Show)

