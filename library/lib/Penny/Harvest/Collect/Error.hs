module Penny.Harvest.Collect.Error where

data T
  = PostingWithoutTopLine
  | TransactionMemoWithoutTopLine
  | PostingMemoWithoutPosting
  deriving (Eq, Ord, Show)

