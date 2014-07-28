module Penny.Pricing.Trade
  ( Trade
  , From(..)
  , To(..)
  , from
  , to
  , trade
  ) where

import Penny.Common

data Trade = Trade
  { from :: From
  , to :: To
  } deriving (Eq, Ord, Show)

-- | Only creates a 'Trade' if the two commodities are different.

trade :: From -> To -> Maybe Trade
trade (From f) (To t)
  | f == t = Nothing
  | otherwise = Just $ Trade (From f) (To t)

newtype From = From { unFrom :: Commodity }
  deriving (Eq, Ord, Show)

newtype To = To { unTo :: Commodity }
  deriving (Eq, Ord, Show)

