-- | Abstract representation of fractional values.
module Penny.Lincoln.Decimal.Frac where

import Deka.Native.Abstract hiding (Exponent(..))
import Penny.Lincoln.Natural
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Groups

-- | A group of zeroes.

newtype Zeroes = Zeroes { unZeroes :: Positive }
  deriving (Eq, Ord, Show)

-- | An MSG (that is, Most Significant Group) that can also have
-- leading zeroes.
data ZeroesMSG = ZeroesMSG
  { zmsgLeadZeroes :: NonNegative
  , zmsgMSG :: MSG
  } deriving (Eq, Ord, Show)

-- | A value that has only a fractional component; that is, its
-- absolute value is greater than zero, but less than one.
data Frac = Frac
  { fracLeadingZero :: Bool
  -- ^ If True, show a zero to the left of the radix point.
  , fracLeading :: [Zeroes]
  , fracMSG :: ZeroesMSG
  , fracLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasExponent Frac where
  exponent (Frac _ _ msg lsgs) =
    Exponent . maybe (error "Frac: error") id
    . nonNegative $ wMSG + wLSGs
    where
      wMSG = let ZeroesMSG _ (MSG _ ds) = msg in length ds + 1
      wLSGs = sum $ map wLSG lsgs
      wLSG (LSG _ ds) = length ds + 1

instance HasDecuple Frac where
  decuple (Frac _ _ msg lsg) =
    let ZeroesMSG _ (MSG nvm ds1) = msg
        lsgDigs (LSG d1 ds) = d1 : ds
    in Decuple nvm (ds1 ++ concatMap lsgDigs lsg)

instance HasCoefficient Frac where
  coefficient = Coefficient . Plenus . decuple
