-- | Metadata to attach to TopLines and Postings. There are many
-- possible ways to handle metadata; previous incarnations of Penny
-- made the TopLine and Posting types parameterizable on the metadata
-- type, the idea being that the nature of metadata might change
-- depending on the way the data was stored. That made a lot of the
-- code unwieldy, for no real benefit at this time because all data is
-- primarily stored the same way. There are, however, other benefits
-- to making the metadata type parameterizable, some theoretical, some
-- practical. For now though the metadata is simply concretely
-- represented in each TopLine and Posting.
module Penny.Lincoln.Meta where

import qualified Penny.Lincoln.Serial as S
import qualified Data.Text as X

-- | The line number that the TopLine starts on (excluding the memo
-- accompanying the TopLine).
newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show)

-- | The line number that the memo accompanying the TopLine starts on.
newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show)

-- | The commodity and and the quantity may appear with the commodity
-- on the left (e.g. USD 2.14) or with the commodity on the right
-- (e.g. 2.14 USD).
data Side = CommodityOnLeft | CommodityOnRight deriving (Eq, Show)

-- | There may or may not be a space in between the commodity and the
-- quantity.
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving (Eq, Show)

data Format =
  Format { side :: Side
         , between :: SpaceBetween }
  deriving (Eq, Show)

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

-- | The line number on which a price appears.
newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show)

-- | The line number on which a posting appears.
newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show)

data PriceMeta =
  PriceMeta { priceLine :: Maybe PriceLine
            , priceFormat :: Maybe Format }
  deriving (Eq, Show)

-- | All postings are numbered in order, beginning with the first
-- posting in the first file and ending with the last posting
-- in the last file.
newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: S.Serial }
  deriving (Eq, Show)

-- | The postings in each file are numbered in order.
newtype FilePosting =
  FilePosting { unFilePosting :: S.Serial }
  deriving (Eq, Show)

emptyPostingMeta :: PostingMeta
emptyPostingMeta = PostingMeta Nothing Nothing Nothing Nothing

-- | All metadata for a posting.
data PostingMeta =
  PostingMeta { postingLine :: Maybe PostingLine
              , postingFormat :: Maybe Format
              , globalPosting :: Maybe GlobalPosting
              , filePosting :: Maybe FilePosting }
  deriving (Eq, Show)

-- | All transactions are numbered in order, beginning with the first
-- transaction in the first file and ending with the last transaction
-- in the last file.
newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: S.Serial }
  deriving (Eq, Show)

-- | The transactions in each file are numbered in order.
newtype FileTransaction =
  FileTransaction { unFileTransaction :: S.Serial }
  deriving (Eq, Show)


emptyTopLineMeta :: TopLineMeta
emptyTopLineMeta = TopLineMeta Nothing Nothing Nothing Nothing Nothing

-- | All metadata accompanying a TopLine.
data TopLineMeta =
  TopLineMeta { topMemoLine :: Maybe TopMemoLine
              , topLineLine :: Maybe TopLineLine
              , filename :: Maybe Filename
              , globalTransaction :: Maybe GlobalTransaction
              , fileTransaction :: Maybe FileTransaction }
  deriving (Eq, Show)

