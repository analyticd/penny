module Penny.Copper.Tree.Posting where

import Penny.Copper.Tree.PreSpace
import Penny.Copper.Tree.Amount
import Penny.Copper.Tree.Commodity
import Penny.Copper.Tree.Currency
import Penny.Copper.Tree.Flag
import Penny.Copper.Tree.Payee.Posting
import Penny.Copper.Tree.Number
import Penny.Copper.Tree.Side
import Penny.Copper.Tree.Tag
import Penny.Copper.Tree.Tokens
import qualified Penny.Copper.Tree.Account.Quoted as AQ
import qualified Penny.Copper.Tree.Account.Unquoted as AU
import Penny.Numbers.Natural

data Item
  = I0 Flag
  | I1 Number
  | I2 Payee
  | I3 AU.Account
  | I4 AQ.Account
  | I5 Tag
  | I6 Debit
  | I7 Credit
  | I8 Commodity
  | I9 Currency
  | I10 AmountPeriod
  | I11 AmountComma
  deriving (Eq, Ord, Show)

newtype Field = Field (PreSpace Item)
  deriving (Eq, Ord, Show)

data Posting = Posting (NE Field Field) Newline
  deriving (Eq, Ord, Show)
