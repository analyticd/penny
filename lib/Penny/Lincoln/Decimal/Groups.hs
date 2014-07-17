-- | Groups of digits.

module Penny.Lincoln.Decimal.Groups where

import Deka.Native

-- | Most significant group.  Must contain a first digit that is not zero.

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

