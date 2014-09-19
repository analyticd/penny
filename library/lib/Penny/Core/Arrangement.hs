module Penny.Core.Arrangement where

import qualified Penny.Core.Orient as O
import qualified Penny.Core.SpaceBetween as S

data T = T
  { orient :: O.T
  , spaceBetween :: S.T
  } deriving (Eq, Ord, Show)
