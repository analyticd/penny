module Penny.Lincoln.Decimal.Whole where

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
-- 'WholeFrac' with an empty 'wfFrac'.
data WholeOnly = WholeOnly
  { woMSG :: MSG
  , woLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasExponent WholeOnly where
  exponent _ = Exponent . maybe (error "Whole: error") id
    . nonNegative $ 0

instance HasDecuple WholeOnly where
  decuple (WholeOnly msg lsgs) = Decuple msd (lsds1 ++ lsdsR)
    where
      MSG msd lsds1 = msg
      lsdsR = concatMap flatten lsgs
      flatten (LSG d1 dr) = d1:dr

instance HasCoefficient WholeOnly where
  coefficient = Coefficient . Plenus . decuple

-- | A number with digits after the radix point.  This will always
-- show a radix point, even if 'wfFrac' is empty.
data WholeFrac = WholeFrac
  { wfMSG :: MSG
  , wfLSG :: [LSG]
  , wfFrac :: [FG]
  } deriving (Eq, Ord, Show)

instance HasExponent WholeFrac where
  exponent (WholeFrac _ _ fs) = Exponent
    . maybe (error "Whole: WholeFrac error") id
    . nonNegative
    . sum
    . map width
    $ fs
    where
      width (FG _ ds) = length ds + 1

instance HasDecuple WholeFrac where
  decuple (WholeFrac (MSG nv ds) lsgs fgs) = Decuple nv (ds ++ rest)
    where
      rest = concatMap flattenLSG lsgs ++ concatMap flattenFG fgs
      flattenLSG (LSG d1 dr) = d1:dr
      flattenFG (FG d1 dr) = d1:dr

instance HasCoefficient WholeFrac where
  coefficient = Coefficient . Plenus . decuple

newtype Whole = Whole { unWhole :: Either WholeOnly WholeFrac }
  deriving (Eq, Ord, Show)

instance HasDecuple Whole where
  decuple = either decuple decuple . unWhole

instance HasCoefficient Whole where
  coefficient = either coefficient coefficient . unWhole

instance HasExponent Whole where
  exponent = either exponent exponent . unWhole

