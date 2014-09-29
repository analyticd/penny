module Penny.Core.Balanced
  ( T
  , fromEnts
  , toSeq
  ) where

import Penny.Core.Balanced.Internal
import qualified Penny.Core.Ents as Ents
import qualified Penny.Core.Imbalances as Imbalances

fromEnts :: Ents.T a -> Maybe (T a)
fromEnts es
  | Imbalances.null . Imbalances.fromBalances . Ents.bals $ es
      = Just . T . Ents.ents $ es
  | otherwise = Nothing
