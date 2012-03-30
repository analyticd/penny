-- | Fills the bottom rows, which contain the tags, memo, and
-- filename. These rows are formatted as follows:
--
-- * If the columns for TotalDrCr, TotalCmdty, and TotalQty
-- are all present, AND if there are at least TWO other columns
-- present, then there will be a hanging indent. The bottom rows will
-- begin at the SECOND column and end with the last column to the left
-- of TotalDrCr.
--
-- * Otherwise, if there are NO columns in the top row, these rows
-- will take the entire width of the report.
--
-- * Otherwise, the bottom rows are as wide as all the top cells
-- combined.


module Penny.Cabin.Posts.BottomRows where
