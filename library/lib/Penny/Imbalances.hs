module Penny.Imbalances where

import qualified Penny.Commodity as Cy
import qualified Data.Map as M
import qualified Penny.Balances as Bal
import qualified Penny.Qty as Qty
import Data.Maybe
import qualified Penny.Concrete as Concrete
import qualified Penny.Quark as Quark
import qualified Penny.Pebble as Pebble

newtype T = T { toMap :: M.Map Cy.T Quark.T }
  deriving (Eq, Ord, Show)

fromBalances :: Bal.T -> T
fromBalances = T . M.fromList . mapMaybe f . M.toList . Bal.toMap
  where
    f (cy, qt) =
      case Quark.fromPebble
      . Pebble.fromCement
      . Concrete.toCement
      . Qty.toConcrete $ qt of
        Nothing -> Nothing
        Just quant -> Just (cy, quant)

toBalances :: T -> Bal.T
toBalances = Bal.T . fmap f . toMap
  where
    f = Qty.T
      . Concrete.fromCement
      . Pebble.toCement
      . Quark.toPebble
