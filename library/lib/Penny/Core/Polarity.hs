module Penny.Core.Polarity where

data T n o p
  = Center n
  | OffCenter o p
  deriving (Eq, Ord, Show)
