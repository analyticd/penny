module Penny.Tree.Date.Error where

data T = T Integer Int Int
         -- ^ The bad year, month, and day
  deriving (Eq, Ord, Show)
