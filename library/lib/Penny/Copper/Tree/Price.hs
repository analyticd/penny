module Penny.Copper.Tree.Price where

import Penny.Copper.Tree.Tokens
import Penny.Copper.Tree.Date
import Penny.Copper.Tree.Currency
import Penny.Copper.Tree.Commodity
import Penny.Copper.Tree.Amount
import Penny.Copper.Tree.PreSpace
import Penny.Copper.Tree.PostSpace

data Price = Price
  (PostSpace Ampersand)
  (PostSpace Date)
  (Maybe (PostSpace Time))
  (PostSpace (Either Currency Commodity))
  (Maybe (PostSpace Commodity))
  (Either AmountPeriod AmountComma)
  (Maybe (PreSpace Commodity))
  Newline
  deriving (Eq,Ord, Show)
