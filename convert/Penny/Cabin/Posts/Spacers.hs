-- | Spacer fields in the report. They don't contain any data; they
-- just provide whitespace. Each spacer immediately follows the named
-- field.
module Penny.Cabin.Posts.Spacers where

data Spacers a = Spacers {
  globalTransaction :: a
  , revGlobalTransaction :: a
  , globalPosting :: a
  , revGlobalPosting :: a
  , fileTransaction :: a
  , revFileTransaction :: a
  , filePosting :: a
  , revFilePosting :: a
  , filtered :: a
  , revFiltered :: a
  , sorted :: a
  , revSorted :: a
  , visible :: a
  , revVisible :: a
  , lineNum :: a
    -- ^ The line number from the posting's metadata
  , date :: a
  , flag :: a
  , number :: a
  , payee :: a
  , account :: a
  , postingDrCr :: a
  , postingCmdty :: a
  , postingQty :: a
  , totalDrCr :: a
  , totalCmdty :: a }
  deriving (Show, Eq)
