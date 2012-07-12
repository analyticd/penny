-- | Spacer fields in the report. They don't contain any data; they
-- just provide whitespace. Each spacer immediately follows the named
-- field.
module Penny.Cabin.Posts.Spacers where

data T a = T {
  globalTransaction :: a
  , globalPosting :: a
  , fileTransaction :: a
  , filePosting :: a
  , filtered :: a
  , sorted :: a
  , visible :: a
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

t_globalTransaction :: a -> T a -> T a
t_globalTransaction a f = f { globalTransaction = a }

t_globalPosting :: a -> T a -> T a
t_globalPosting a f = f { globalPosting = a }

t_fileTransaction :: a -> T a -> T a
t_fileTransaction a f = f { fileTransaction = a }

t_filePosting :: a -> T a -> T a
t_filePosting a f = f { filePosting = a }

t_filtered :: a -> T a -> T a
t_filtered a f = f { filtered = a }

t_sorted :: a -> T a -> T a
t_sorted a f = f { sorted = a }

t_visible :: a -> T a -> T a
t_visible a f = f { visible = a }

t_lineNum :: a -> T a -> T a
t_lineNum a f = f { lineNum = a }

t_date :: a -> T a -> T a
t_date a f = f { date = a }

t_flag :: a -> T a -> T a
t_flag a f = f { flag = a }

t_number :: a -> T a -> T a
t_number a f = f { number = a }

t_payee :: a -> T a -> T a
t_payee a f = f { payee = a }

t_account :: a -> T a -> T a
t_account a f = f { account = a }

t_postingDrCr :: a -> T a -> T a
t_postingDrCr a f = f { postingDrCr = a }

t_postingCmdty :: a -> T a -> T a
t_postingCmdty a f = f { postingCmdty = a }

t_postingQty :: a -> T a -> T a
t_postingQty a f = f { postingQty = a }

t_totalDrCr :: a -> T a -> T a
t_totalDrCr a f = f { totalDrCr = a }

t_totalCmdty :: a -> T a -> T a
t_totalCmdty a f = f { totalCmdty = a }
