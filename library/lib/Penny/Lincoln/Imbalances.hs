module Penny.Lincoln.Imbalances where

import qualified Data.Map as M
import Penny.Lincoln.DecNonZero
import Penny.Lincoln.Balances
import Penny.Lincoln.Commodity
import Data.Maybe (mapMaybe)
import Penny.Lincoln.Qty

newtype Imbalances = Imbalances (M.Map Commodity DecNonZero)
  deriving (Eq, Ord, Show)

balancesToImbalances :: Balances -> Imbalances
balancesToImbalances (Balances m)
  = Imbalances
  . M.fromList
  . mapMaybe f . M.toList $ m
  where
    f (cy, (Qty q)) = case decimalToDecNonZero q of
      Nothing -> Nothing
      Just dnz -> Just (cy, dnz)
