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
