-- | Various low-level components of prices, postings, and top lines.
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

-- | There is one item in the list for each line of the memo. Do not
-- include newlines in the texts themselves. However there is nothing
-- to enforce this convention.
newtype Memo = Memo { unMemo :: [Text] }
             deriving (Eq, Show, Ord)

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

-- | The location of an item.  In a file, this is generally going to
-- be a line number.
newtype Location = Location { unLocation :: Int }
  deriving (Eq, Ord, Show)

-- | A name for a collection of transactions.  If they are from a
-- file, this will generally be the filename.
newtype Clxn = Clxn { unClxn :: Text }
  deriving (Eq, Ord, Show)

