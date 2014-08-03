module Penny.Balance.Shrinkers where

import Penny.Balance
import Data.Map.Shrinkers
import Prelude hiding (map, exponent)
import Penny.Common.Shrinkers
import Penny.Numbers.Qty.Shrinkers
import Prelude.Shrinkers
import Penny.Numbers.Abstract.Unpolar.Shrinkers

balances :: Balances -> [Balances]
balances (Balances m) = fmap Balances $ map (tuple2 commodity qty) m

nonZero :: NonZero -> [NonZero]
nonZero (NonZero n e s) =
  [ NonZero n' e' s' | (n', e', s') <-
    tuple3 novDecs exponent side (n, e, s) ]

imbalances :: Imbalances -> [Imbalances]
imbalances (Imbalances m) = fmap Imbalances $
  map (tuple2 commodity nonZero) m
