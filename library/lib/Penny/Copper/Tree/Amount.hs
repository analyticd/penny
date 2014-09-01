module Penny.Copper.Tree.Amount where

import Penny.Copper.Tree.Unsigned
import Penny.Copper.Tree.Currency
import Penny.Numbers.Abstract.RadGroup
import Penny.Copper.Tree.Tokens

data PreCurrency r = PreCurrency Currency (Start r)
  deriving (Eq, Ord, Show)

data PostCurrency r = PostCurrency (Start r) (Maybe Currency)
  deriving (Eq, Ord, Show)

data AmountPeriod
  = APCurrency (PreCurrency Period)
  | APStart (PostCurrency Period)
  deriving (Eq, Ord, Show)

data AmountComma = AmountComma Apostrophe AC2
  deriving (Eq, Ord, Show)

data AC2
  = AC2Currency (PreCurrency Comma) Apostrophe
  | AC2Start (PostCurrency Comma) Apostrophe
  deriving (Eq, Ord, Show)
