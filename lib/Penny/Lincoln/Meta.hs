module Penny.Lincoln.Meta where

import Data.Bifunctor
import Penny.Lincoln.Serial
import Data.Text (Text)

-- | Bundles an item up with its metadata.
data Bundle p m = Bundle
  { payload :: p
  , meta :: m
  } deriving (Eq, Ord, Show)

instance Bifunctor Bundle where
  bimap f g (Bundle p m) = Bundle (f p) (g m)

-- | The line something appears on in a file.
newtype Line = Line { unLine :: Int }
  deriving (Eq, Ord, Show)

-- | The commodity and the representation may appear with the commodity
-- on the left (e.g. USD 2.14) or with the commodity on the right
-- (e.g. 2.14 USD).
data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Show, Ord)

-- | There may or may not be a space in between the commodity and the
-- representation.
data SpaceBetween
  = SpaceBetween
  | NoSpaceBetween
  deriving (Eq, Show, Ord)

data Arrangement = Arrangement
  { orient :: Orient
  , spaceBetween :: SpaceBetween
  } deriving (Eq, Ord, Show)

-- | All postings are numbered in order, beginning with the first
-- posting in the first file and ending with the last posting
-- in the last file.
newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: Serial }
  deriving (Eq, Show)

-- | The postings in each file are numbered in order.
newtype FilePosting =
  FilePosting { unFilePosting :: Serial }
  deriving (Eq, Show)

-- | All transactions are numbered in order, beginning with the first
-- transaction in the first file and ending with the last transaction
-- in the last file.
newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: Serial }
  deriving (Eq, Show)

-- | The transactions in each file are numbered in order.
newtype FileTransaction =
  FileTransaction { unFileTransaction :: Serial }
  deriving (Eq, Show)

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: Text }
  deriving (Eq, Show)

