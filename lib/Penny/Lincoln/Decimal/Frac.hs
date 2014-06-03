module Penny.Lincoln.Decimal.Frac where

import Deka.Native.Abstract hiding (Exponent(..))
import Penny.Lincoln.Natural
import Penny.Lincoln.Decimal.Components

-- | A group of zeroes.

newtype Zeroes = Zeroes { unZeroes :: Positive }
  deriving (Eq, Ord, Show)

data MSG = MSG
  { msgLeadZeroes :: NonNegative
  , msgMSD :: Novem
  , msgLSD :: [Decem]
  } deriving (Eq, Ord, Show)

data LSG = LSG
  { lsgFirst :: Decem
  , lsgRest :: [Decem]
  } deriving (Eq, Ord, Show)

data Frac = Frac
  { leadingZero :: Bool
  -- ^ If True, show a zero to the left of the radix point.
  , fracLeading :: [Zeroes]
  , fracMSG :: MSG
  , fracLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasExponent Frac where
  exponent (Frac _ _ msg lsgs) =
    Exponent . maybe (error "Frac: error") id
    . nonNegative $ wMSG + wLSGs
    where
      wMSG = let MSG _ _ ds = msg in length ds + 1
      wLSGs = sum $ map wLSG lsgs
      wLSG (LSG _ ds) = length ds + 1

instance HasDecuple Frac where
  decuple (Frac _ _ msg lsg) =
    let MSG _ nvm ds1 = msg
        lsgDigs (LSG d1 ds) = d1 : ds
    in Decuple nvm (ds1 ++ concatMap lsgDigs lsg)

instance HasCoefficient Frac where
  coefficient = Coefficient . Plenus . decuple
