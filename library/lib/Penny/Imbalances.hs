module Penny.Imbalances where

import qualified Penny.Commodity as Cy
import qualified Penny.Quant as Quant
import qualified Data.Map as M
import qualified Penny.Balances as Bal
import qualified Penny.Qty as Qty
import Data.Maybe
import qualified Penny.Gravel as Gravel
import qualified Penny.Concrete as Concrete
import qualified Penny.Side as Side

newtype T = T { toMap :: M.Map Cy.T (Quant.T Side.T) }
  deriving (Eq, Ord, Show)

fromBalances :: Bal.T -> T
fromBalances = T . M.fromList . mapMaybe f . M.toList . Bal.toMap
  where
    f (cy, qt) = case Quant.fromGravel
      . Gravel.fromCement Side.fromSign
      . Concrete.toCement
      . Qty.toConcrete $ qt of
      Nothing -> Nothing
      Just quant -> Just (cy, quant)

toBalances :: T -> Bal.T
toBalances = Bal.T . fmap f . toMap
  where
    f = Qty.fromConcrete
      . Concrete.fromCement
      . Gravel.toCement Side.toSign
      . Quant.toGravel
