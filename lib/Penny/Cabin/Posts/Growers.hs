-- | Calculates cells that "grow to fit." These cells grow to fit the
-- widest cell in the column. No information is ever truncated from
-- these cells (what use is a truncated dollar amount?)
module Penny.Cabin.Posts.Growers (growCells, Fields) where

import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Row as R

growCells :: Options.T a -> [Info.T] -> [Fields (Maybe R.Cell)]
growCells o = justifyCells . map (getCells o)

justifyCells :: [Fields (Maybe R.Cell)] -> [Fields (Maybe R.Cell)]
justifyCells = undefined

getCells :: Options.T a -> Info.T -> Fields (Maybe R.Cell)
getCells os i = let
  flds = growingFields os
  ifShown fn a =
    if fn flds then Just a else Nothing in
  Fields {
    postingNum = ifShown postingNum (getPostingNum os i)
    , visibleNum = ifShown visibleNum (getVisibleNum os i)
    , revPostingNum = ifShown revPostingNum (getRevPostingNum os i)
    , lineNum = ifShown lineNum (getLineNum os i)
    , date = ifShown date (getDate os i)
    , flag = ifShown flag (getFlag os i)
    , number = ifShown number (getNumber os i)
    , postingDrCr = ifShown postingDrCr (getPostingDrCr os i)
    , postingCmdty = ifShown postingCmdty (getPostingCmdty os i)
    , postingQty = ifShown postingQty (getPostingQty os i)
    , totalDrCr = ifShown totalDrCr (getTotalDrCr os i)
    , totalCmdty = ifShown totalCmdty (getTotalCmdty os i)
    , totalQty = ifShown totalQty (getTotalQty os i) }


getPostingNum :: Options.T a -> Info.T -> R.Cell
getPostingNum = undefined

getVisibleNum :: Options.T a -> Info.T -> R.Cell
getVisibleNum = undefined

getRevPostingNum :: Options.T a -> Info.T -> R.Cell
getRevPostingNum = undefined

getLineNum :: Options.T a -> Info.T -> R.Cell
getLineNum = undefined

getDate :: Options.T a -> Info.T -> R.Cell
getDate = undefined

getFlag :: Options.T a -> Info.T -> R.Cell
getFlag = undefined

getNumber :: Options.T a -> Info.T -> R.Cell
getNumber = undefined

getPostingDrCr :: Options.T a -> Info.T -> R.Cell
getPostingDrCr = undefined

getPostingCmdty :: Options.T a -> Info.T -> R.Cell
getPostingCmdty = undefined

getPostingQty :: Options.T a -> Info.T -> R.Cell
getPostingQty = undefined

getTotalDrCr :: Options.T a -> Info.T -> R.Cell
getTotalDrCr = undefined

getTotalCmdty :: Options.T a -> Info.T -> R.Cell
getTotalCmdty = undefined

getTotalQty :: Options.T a -> Info.T -> R.Cell
getTotalQty = undefined

growingFields :: Options.T a -> Fields Bool
growingFields = undefined

-- | All growing fields.
data Fields a = Fields {
  postingNum :: a
  , visibleNum :: a
  , revPostingNum :: a
  , lineNum :: a
    -- ^ The line number from the posting's metadata
  , date :: a
  , flag :: a
  , number :: a
  , postingDrCr :: a
  , postingCmdty :: a
  , postingQty :: a
  , totalDrCr :: a
  , totalCmdty :: a
  , totalQty :: a }
  deriving (Show, Eq)

{-
t_postingNum :: a -> Fields a -> Fields a
t_postingNum a f = f { postingNum = a }

t_visibleNum :: a -> Fields a -> Fields a
t_visibleNum a f = f { visibleNum = a }

t_revPostingNum :: a -> Fields a -> Fields a
t_revPostingNum a f = f { revPostingNum = a }

t_lineNum :: a -> Fields a -> Fields a
t_lineNum a f = f { lineNum = a }

t_date :: a -> Fields a -> Fields a
t_date a f = f { date = a }

t_flag :: a -> Fields a -> Fields a
t_flag a f = f { flag = a }

t_number :: a -> Fields a -> Fields a
t_number a f = f { number = a }

t_postingDrCr :: a -> Fields a -> Fields a
t_postingDrCr a f = f { postingDrCr = a }

t_postingCmdty :: a -> Fields a -> Fields a
t_postingCmdty a f = f { postingCmdty = a }

t_postingQty :: a -> Fields a -> Fields a
t_postingQty a f = f { postingQty = a }

t_totalDrCr :: a -> Fields a -> Fields a
t_totalDrCr a f = f { totalDrCr = a }

t_totalCmdty :: a -> Fields a -> Fields a
t_totalCmdty a f = f { totalCmdty = a }

t_totalQty :: a -> Fields a -> Fields a
t_totalQty a f = f { totalQty = a }

-}
