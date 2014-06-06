module Penny.Lincoln.Decimal.Groups where

import Deka.Native

-- | Most significant group.

data MSG = MSG
  { msgMSD :: Novem
  -- ^ Most sigificant digit

  , msdLSD :: [Decem]
  -- ^ Less significant digits
  } deriving (Eq, Ord, Show)

-- | Less significant groups.

data LSG = LSG
  { lsgFirst :: Decem
  , lsgRest :: [Decem]
  } deriving (Eq, Ord, Show)

