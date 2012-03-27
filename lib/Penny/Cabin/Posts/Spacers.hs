-- | Spacer fields in the report. They don't contain any data; they
-- just provide whitespace. Each spacer immediately follows the named
-- field.
module Penny.Cabin.Posts.Spacers where

data T a = T {
  postingNum :: a
  , visibleNum :: a
  , revPostingNum :: a
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

t_postingNum :: a -> T a -> T a
t_postingNum a f = f { postingNum = a }

t_visibleNum :: a -> T a -> T a
t_visibleNum a f = f { visibleNum = a }

t_revPostingNum :: a -> T a -> T a
t_revPostingNum a f = f { revPostingNum = a }

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
