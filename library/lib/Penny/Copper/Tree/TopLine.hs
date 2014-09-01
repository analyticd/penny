module Penny.Copper.Tree.TopLine where

import Penny.Copper.Tree.Date
import Penny.Copper.Tree.Payee.TopLine
import Penny.Copper.Tree.Number
import Penny.Copper.Tree.Flag
import Penny.Copper.Tree.PostSpace
import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

data Item
  = I0 (PostSpace Date)
  | I1 (PostSpace Time)
  | I2 (PostSpace Flag)
  | I3 (PostSpace Number)
  deriving (Eq, Ord, Show)

data TopLine = TopLine (Seq Item) (Maybe Payee) Newline
  deriving (Eq, Ord, Show)
