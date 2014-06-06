-- | Items (both metadata and payload) common to both prices and
-- transactions.

module Penny.Lincoln.Common
  ( module Penny.Lincoln.Common.DateTime
  , Commodity(..)
  , Bundle(..)
  , Line(..)
  , Orient(..)
  , SpaceBetween(..)
  , Arrangement(..)
  , Filename(..)
  ) where

import Data.Text (Text)
import Penny.Lincoln.Common.DateTime
import Data.Bifunctor

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

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

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: Text }
  deriving (Eq, Ord, Show)

