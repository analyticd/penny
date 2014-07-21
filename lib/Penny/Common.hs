module Penny.Common where

import Data.Text (Text)

newtype Commodity =
  Commodity { unCommodity :: Text }
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

