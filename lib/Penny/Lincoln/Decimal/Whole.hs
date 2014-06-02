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

instance HasWidth MSG where
  width (MSG _ ds) = length ds + 1

-- | Less significant groups.

data LSG = LSG
  { lsgFirst :: Decem
  , lsgRest :: [Decem]
  } deriving (Eq, Ord, Show)

instance HasWidth LSG where
  width (LSG _ rs) = length rs + 1

-- | Fractional groups.

data FG = FG
  { fgDigit1 :: Decem
  , fgDigitRest :: [Decem]
  } deriving (Eq, Ord, Show)

instance HasWidth FG where
  width (FG _ ds) = length ds + 1

-- | A number with no digits after the radix point.  This will never
-- show a radix point.  To create a whole number with a trailing
-- radix point but no digits after the decimal point, use
-- 'WholeFrac' with an empty 'wfFrac'.
data WholeOnly = WholeOnly
  { woMSG :: MSG
  , woLSG :: [LSG]
  } deriving (Eq, Ord, Show)

instance HasWidth WholeOnly where
  width (WholeOnly msg lsgs) = width msg + sum (map width lsgs)

instance HasExponent WholeOnly where
  exponent _ = Exponent . maybe (error "Whole: error") id
    . nonNegative $ 0

instance HasCoefficient WholeOnly where
  coefficient (WholeOnly msg lsgs) = Coefficient $ Plenus dc
    where
      dc = Decuple msd (lsds1 ++ lsdsR)
      MSG msd lsds1 = msg
      lsdsR = concatMap flatten lsgs
      flatten (LSG d1 dr) = d1:dr

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

instance HasCoefficient WholeFrac where
  coefficient (WholeFrac (MSG nv ds) lsgs fgs) =
    Coefficient (Plenus dc)
    where
      dc = Decuple nv (ds ++ rest)
      rest = concatMap flattenLSG lsgs ++ concatMap flattenFG fgs
      flattenLSG (LSG d1 dr) = d1:dr
      flattenFG (FG d1 dr) = d1:dr

instance HasWidth WholeFrac where
  width (WholeFrac msg lsgs fgs) = width msg + sum (map width lsgs)
    + sum (map width fgs)

newtype Whole = Whole { unWhole :: Either WholeOnly WholeFrac }
  deriving (Eq, Ord, Show)

instance HasCoefficient Whole where
  coefficient = either coefficient coefficient . unWhole

instance HasExponent Whole where
  exponent = either exponent exponent . unWhole

instance HasWidth Whole where
  width = either width width . unWhole
