module Penny.Lincoln.Arrangement where

import qualified Penny.Lincoln.Orient as O
import qualified Penny.Lincoln.SpaceBetween as S

data T = T
  { orient :: O.T
  , spaceBetween :: S.T
  } deriving (Eq, Ord, Show)
