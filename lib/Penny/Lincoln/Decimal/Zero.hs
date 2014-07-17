-- | Abstract representations of zeroes.

module Penny.Lincoln.Decimal.Zero where

import Deka.Native.Abstract hiding (Exponent(..))
import Penny.Lincoln.Natural
import Penny.Lincoln.Decimal.Components
import Prelude hiding (exponent)

-- | A plain zero, with no trailing radix point.

data PlainZero = PlainZero
  deriving (Eq, Ord, Show)

instance HasExponent PlainZero where
  exponent _ = Exponent . maybe (error "PlainZero: error") id
    . nonNegative $ 0

instance HasCoefficient PlainZero where
  coefficient _ = Coefficient Nil

newtype Group = Group { unGroup :: Positive }
  deriving (Eq, Ord, Show)

-- | A zero with groups of zeroes after the radix point.

data GroupedZero = GroupedZero
  { gzLzeadingZero :: Bool
  -- ^ If True, show a zero to the left of the radix point.
  , group1 :: Group
  , groupRest :: [Group]
  } deriving (Eq, Ord, Show)

instance HasExponent GroupedZero where
  exponent (GroupedZero _ g1 gr) = Exponent
    . maybe (error "GroupedZero: error") id . nonNegative
    $ len g1 + sum (map len gr)
    where
      len = unPositive . unGroup

instance HasCoefficient GroupedZero where
  coefficient _ = Coefficient Nil

newtype Zero = Zero { unZero :: Either PlainZero GroupedZero }
  deriving (Eq, Ord, Show)

instance HasCoefficient Zero where
  coefficient _ = Coefficient Nil

instance HasExponent Zero where
  exponent = either exponent exponent . unZero
