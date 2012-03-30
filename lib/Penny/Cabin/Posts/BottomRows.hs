-- | Fills the bottom rows, which contain the tags, memo, and
-- filename. These rows are formatted as follows:
--
-- * If the columns for TotalDrCr, TotalCmdty, and TotalQty are all
-- present, AND if there are at least TWO other columns present, then
-- there will be a hanging indent. The bottom rows will begin at the
-- SECOND column and end with the last column to the left of
-- TotalDrCr. In this case, each bottom row will have three cells: one
-- padding on the left, one main content, and one padding on the
-- right.
--
-- * Otherwise, if there are NO columns in the top row, these rows
-- will take the entire width of the report. Each bottom row will have
-- one cell.
--
-- * Otherwise, the bottom rows are as wide as all the top cells
-- combined. Each bottom row will have one cell.


module Penny.Cabin.Posts.BottomRows where

import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.Fields as Fields
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Options as O

data Fields a = Fields {
  tags :: a
  , memo :: a
  , filename :: a
  } deriving (Show, Eq)

bottomRowsFields :: Fields.T a -> Fields a
bottomRowsFields f = Fields {
  tags = F.tags f
  , memo = F.memo f
  , filename = F.filename f }

data BottomLayout =
  HangingIndent
  | WidthOfTopColumns
  | WidthOfReport
  deriving (Show, Eq)
