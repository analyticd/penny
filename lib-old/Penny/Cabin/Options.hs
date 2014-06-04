-- | Options applicable to multiple Cabin reports.

module Penny.Cabin.Options where

-- | Whether to show zero balances in reports.
newtype ShowZeroBalances =
  ShowZeroBalances { unShowZeroBalances :: Bool }
  deriving (Show, Eq)

-- | Converts an ordering to a descending order.
descending :: (a -> a -> Ordering)
              -> a -> a -> Ordering
descending f x y = case f x y of
  LT -> GT
  GT -> LT
  EQ -> EQ
