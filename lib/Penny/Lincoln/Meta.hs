module Penny.Lincoln.Meta where

import qualified Penny.Lincoln.Serial as S
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

newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show)

newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show)

data PriceMeta =
  PriceMeta { priceLine :: Maybe PriceLine
            , priceFormat :: Maybe Format }
  deriving (Eq, Show)

newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: S.Serial }
  deriving (Eq, Show)

newtype FilePosting =
  FilePosting { unFilePosting :: S.Serial }
  deriving (Eq, Show)

emptyPostingMeta :: PostingMeta
emptyPostingMeta = PostingMeta Nothing Nothing Nothing Nothing

data PostingMeta =
  PostingMeta { postingLine :: Maybe PostingLine
              , postingFormat :: Maybe Format
              , globalPosting :: Maybe GlobalPosting
              , filePosting :: Maybe FilePosting }
  deriving (Eq, Show)

newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: S.Serial }
  deriving (Eq, Show)

newtype FileTransaction =
  FileTransaction { unFileTransaction :: S.Serial }
  deriving (Eq, Show)


emptyTopLineMeta :: TopLineMeta
emptyTopLineMeta = TopLineMeta Nothing Nothing Nothing Nothing Nothing

data TopLineMeta =
  TopLineMeta { topMemoLine :: Maybe TopMemoLine
              , topLineLine :: Maybe TopLineLine
              , filename :: Maybe Filename
              , globalTransaction :: Maybe GlobalTransaction
              , fileTransaction :: Maybe FileTransaction }
  deriving (Eq, Show)

