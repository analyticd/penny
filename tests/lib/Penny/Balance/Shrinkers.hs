module Penny.Balance.Shrinkers where

import Penny.Balance
import Data.Map.Shrinkers
import Prelude hiding (map)
import Penny.Common.Shrinkers
import Penny.Numbers.Qty.Shrinkers
import Prelude.Shrinkers

balances :: Balances -> [Balances]
balances (Balances m) = fmap Balances $ map (tuple2 commodity qty) m
