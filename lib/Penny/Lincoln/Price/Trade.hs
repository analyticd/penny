module Penny.Lincoln.Bits.Trade
  ( Trade
  , from
  , to
  , trade
  ) where

import Penny.Lincoln.Bits.Open

data Trade = Trade
  { from :: From
  , to :: To
  } deriving (Eq, Ord, Show)

-- | Only creates a 'Trade' if the two commodities are different.

trade :: From -> To -> Maybe Trade
trade (From f) (To t)
  | f == t = Nothing
  | otherwise = Just $ Trade (From f) (To t)
