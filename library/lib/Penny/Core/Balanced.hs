module Penny.Core.Balanced
  ( T
  , fromEnts
  , toSeq
  , empty
  ) where

import Penny.Core.Balanced.Internal
import qualified Penny.Core.Ents as Ents
import qualified Penny.Core.Imbalances as Imbalances
import qualified Data.Sequence as S

fromEnts :: Ents.T a -> Either Imbalances.T (T a)
fromEnts es
  | Imbalances.null imb = Right . T . Ents.ents $ es
  | otherwise = Left imb
  where
    imb = Imbalances.fromBalances . Ents.bals $ es

empty :: T a
empty = T S.empty
