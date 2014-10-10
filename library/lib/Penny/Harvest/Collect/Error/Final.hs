module Penny.Harvest.Collect.Error.Final where

-- | Errors that occur after all lines are processed.
data T
  = TransactionMemoWithoutTopLine
  deriving (Eq, Ord, Show)
