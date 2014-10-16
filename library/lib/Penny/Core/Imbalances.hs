module Penny.Core.Imbalances where

import qualified Penny.Core.Commodity as Cy
import qualified Data.Map as M
import qualified Penny.Core.Balances as Bal
import qualified Penny.Core.Qty as Qty
import Data.Maybe
import qualified Penny.Core.Cement as Cement
import qualified Penny.Core.Quark as Quark
import qualified Penny.Core.Pebble as Pebble

newtype T = T { toMap :: M.Map Cy.T Quark.T }
  deriving (Eq, Ord, Show)

fromBalances :: Bal.T -> T
fromBalances = T . M.fromList . mapMaybe f . M.toList . Bal.toMap
  where
    f (cy, qt) =
      case Quark.fromPebble
      . Pebble.fromCement
      . Cement.fromConcrete
      . Qty.toConcrete $ qt of
        Nothing -> Nothing
        Just quant -> Just (cy, quant)

toBalances :: T -> Bal.T
toBalances = Bal.T . fmap f . toMap
  where
    f = Qty.T
      . Cement.toConcrete
      . Pebble.toCement
      . Quark.toPebble

null :: T -> Bool
null (T m) = M.null m
