-- | A generic type that represents all the available fields in the
-- postings report. Its current main use is parameterized on Bool to
-- indicate which fields the user wants to see.
module Penny.Cabin.Postings.Fields where

data Fields a =
  Fields { lineNum :: a
         , date :: a
         , flag :: a
         , number :: a
         , payee :: a
         , account :: a
         , postingDrCr :: a
         , postingCmdty :: a
         , postingQty :: a
         , totalDrCr :: a
         , totalCmdty :: a
         , totalQty :: a
         , tags :: a
         , memo :: a
         , filename :: a }
  deriving (Show, Eq)

t_lineNum :: a -> Fields a -> Fields a
t_lineNum a f = f { lineNum = a }

t_date :: a -> Fields a -> Fields a
t_date a f = f { date = a }

t_flag :: a -> Fields a -> Fields a
t_flag a f = f { flag = a }

t_number :: a -> Fields a -> Fields a
t_number a f = f { number = a }

t_payee :: a -> Fields a -> Fields a
t_payee a f = f { payee = a }

t_account :: a -> Fields a -> Fields a
t_account a f = f { account = a }

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

t_tags :: a -> Fields a -> Fields a
t_tags a f = f { tags = a }

t_memo :: a -> Fields a -> Fields a
t_memo a f = f { memo = a }

t_filename :: a -> Fields a -> Fields a
t_filename a f = f { filename = a }

