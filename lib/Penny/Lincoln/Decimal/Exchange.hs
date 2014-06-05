{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Lincoln.Decimal.Exchange where

import Penny.Lincoln.Decimal.Multiplier
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Equivalent
import Deka.Dec(PosNeg(..))
import Penny.Lincoln.Decimal.Amount
import qualified Penny.Lincoln.Decimal.Concrete as C
import qualified Penny.Lincoln.Decimal.Multiplier as M

data Exchange
  = EAbstract (Abstract PosNeg)
  | EMultiplier Multiplier
  deriving (Eq, Ord, Show)

instance HasMultiplier Exchange where
  multiplier r = case r of
    EAbstract d -> multiplier d
    EMultiplier c -> multiplier c

instance Laned Exchange PosNeg where
  lane r = case r of
    EAbstract a -> lane a
    EMultiplier c -> lane c

-- | Two 'Exchange' are equivalent if, after converting any abstract
-- representation to a 'Multiplier', the 'Multiplier'
-- are equivalent.

instance Equivalent Exchange where
  compareEv x y = compareEv xc yc
    where
      xc = case x of
        EAbstract a -> multiplier a
        EMultiplier a -> a
      yc = case y of
        EAbstract a -> multiplier a
        EMultiplier a -> a

-- | Converts the 'Amount' to a new 'Amount' using the given
-- 'Exchange'.  For example, if @a@ is an 'Amount' representing a
-- 'Debit' entry of 5 apples, and @e@ is an 'Exchange' representing
-- that 1 apple is worth 3 dollars, the result is an 'Amount' that
-- is a 'Debit' entry of 15, which represents 15 dollars.
--
-- If the 'Exchange' is 'Neg', then the resulting 'Amount' will be
-- the opposite of the original 'Amount'.  Thus, if @a@ is an
-- 'Amount' representing a 'Debit' entry of 5 apples, and @e@ is an
-- 'Exchange' representing that 1 apple is worth /negative/ 3
-- dollars, the result is an 'Amount' that is a 'Credit' entry of 15
-- dollars.  (This is rather like your apples being a liability,
-- like toxic waste.)

convert :: Exchange -> Amount -> Amount
convert e a = AConcrete c'
  where
    c' = changeSign $ C.mult cAmount cExch
    changeSign = case lane e of
      Center -> id
      NonCenter (s, _) -> case s of
        Pos -> id
        Neg -> C.negate
    cAmount = C.concrete a
    cExch = C.Concrete . M.unMultiplier
      . multiplier $ e
