module Penny.Numbers.Allocator where

import Penny.Numbers.Unsigned
import qualified Data.Foldable as F

-- | Represents a number using native Haskell Integers.  Represents
-- positive numbers only; these are all that the allocator can deal
-- with.

data Nabst = Nabst
  { naCoeff :: Unsigned
  , naExponent :: Unsigned
  -- ^ The exponent is either zero or negative
  } deriving (Eq, Ord, Show)

-- | Subtracts the given number from the exponent.  This makes the
-- exponent smaller than before; though the 'Unsigned' that represents
-- the exponent will be larger, remember that all exponents are either
-- zero or negative.  Also, adjusts the coefficient accordingly.

subtractFromExponent :: Unsigned -> Nabst -> Nabst
subtractFromExponent n (Nabst c e) =
  Nabst (c `mult` (ten `pow` n)) (e `add` n)

-- | Equalizes all exponents in a sequence.

equalizeExponents :: (Functor f, F.Foldable f) => f Nabst -> f Nabst
equalizeExponents ls = fmap adjust ls
  where
    target = F.maximum . fmap naExponent $ ls
    adjust n = subtractFromExponent diff n
      where
        diff = monus target (naExponent n)
