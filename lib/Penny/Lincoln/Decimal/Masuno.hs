-- | Unsigned abstract numbers whose value is greater than or equal
-- to one.
module Penny.Lincoln.Decimal.Masuno where

import Deka.Native.Abstract hiding (Exponent(..))
import Prelude hiding (exponent)
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Natural

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

-- | Fractional groups.

data FG = FG
  { fgDigit1 :: Decem
  , fgDigitRest :: [Decem]
  } deriving (Eq, Ord, Show)

-- | A number with no digits after the radix point.  This will never
-- show a radix point.  To create a whole number with a trailing
-- radix point but no digits after the decimal point, use
-- 'Fracuno' with an empty 'wfFrac'.
data Monly = Monly
  { moMSG :: MSG
  , moLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasExponent Monly where
  exponent _ = Exponent . maybe (error "Masuno: error") id
    . nonNegative $ 0

instance HasDecuple Monly where
  decuple (Monly msg lsgs) = Decuple msd (lsds1 ++ lsdsR)
    where
      MSG msd lsds1 = msg
      lsdsR = concatMap flatten lsgs
      flatten (LSG d1 dr) = d1:dr

instance HasCoefficient Monly where
  coefficient = Coefficient . Plenus . decuple

-- | A number with digits after the radix point.  This will always
-- show a radix point, even if 'fcFrac' is empty.
data Fracuno = Fracuno
  { fcMSG :: MSG
  , fcLSG :: [LSG]
  , fcFrac :: [FG]
  } deriving (Eq, Ord, Show)

instance HasExponent Fracuno where
  exponent (Fracuno _ _ fs) = Exponent
    . maybe (error "Masuno: Fracuno error") id
    . nonNegative
    . sum
    . map width
    $ fs
    where
      width (FG _ ds) = length ds + 1

instance HasDecuple Fracuno where
  decuple (Fracuno (MSG nv ds) lsgs fgs) = Decuple nv (ds ++ rest)
    where
      rest = concatMap flattenLSG lsgs ++ concatMap flattenFG fgs
      flattenLSG (LSG d1 dr) = d1:dr
      flattenFG (FG d1 dr) = d1:dr

instance HasCoefficient Fracuno where
  coefficient = Coefficient . Plenus . decuple

-- | An unsigned abstract number whose value that is greater than or
-- equal to one.  (name origin: /más uno/)
newtype Masuno = Masuno { unMasuno :: Either Monly Fracuno }
  deriving (Eq, Ord, Show)

instance HasDecuple Masuno where
  decuple = either decuple decuple . unMasuno

instance HasCoefficient Masuno where
  coefficient = either coefficient coefficient . unMasuno

instance HasExponent Masuno where
  exponent = either exponent exponent . unMasuno

