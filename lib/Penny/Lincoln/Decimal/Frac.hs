module Penny.Lincoln.Decimal.Frac where

import Deka.Native.Abstract
import Penny.Lincoln.Natural
import Penny.Lincoln.Decimal.Components

-- | A group of zeroes.

newtype Zeroes = Zeroes { unZeroes :: Positive }
  deriving (Eq, Ord, Show)

instance HasWidth Zeroes where
  width (Zeroes p) = unPositive p

data MSG = MSG
  { msgLeadZeroes :: NonNegative
  , msgMSD :: Novem
  , msgLSD :: [Decem]
  } deriving (Eq, Ord, Show)

instance HasWidth MSG where
  width (MSG lz _ lsd) = unNonNegative lz + 1 + length lsd

data LSG = LSG
  { lsgFirst :: Decem
  , lsgRest :: [Decem]
  } deriving (Eq, Ord, Show)

instance HasWidth LSG where
  width (LSG _ ds) = length ds + 1

data Frac = Frac
  { leadingZero :: Bool
  -- ^ If True, show a zero to the left of the radix point.
  , fracLeading :: [Zeroes]
  , fracMSG :: MSG
  , fracLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasWidth Frac where
  width (Frac _ zs msg lsgs) = sum (map width zs) + width msg +
    sum (map width lsgs)
