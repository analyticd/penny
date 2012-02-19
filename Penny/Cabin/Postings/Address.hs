module Penny.Cabin.Postings.Address where

import Data.Ix (Ix)

data Col =
  LineNum
  | SLineNum
  | Date
  | SDate
  | Multi  -- Row 0: Flag; Row 1: Tags; Row 2: Memo; Row 3: Filename
  | SMulti
  | Num
  | SNum
  | Payee
  | SPayee
  | Account
  | SAccount
  | PostingDrCr
  | SPostingDrCr
  | PostingCommodity
  | SPostingCommodity
  | PostingQty
  | SPostingQty
  | TotalDrCr
  | STotalDrCr
  | TotalCommodity
  | STotalCommodity
  | TotalQty
    deriving (Eq, Ord, Show, Ix, Bounded)

data Row =
  Top
  | Tags
  | Memo
  | Filename
  deriving (Eq, Ord, Show, Ix, Bounded)
