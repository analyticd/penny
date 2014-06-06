-- | The 'Concrete' data type and associated functions.  These
-- are concrete numbers, together with a 'Lane' (that is, whether it
-- is a debit, credit, or zero.)  These numbers cannot be rendered
-- as strings; for that, you will need to convert them to a 'Rep'.
-- However, 'Concrete' types are the only ones with which you can
-- perform arithmetic.
module Penny.Lincoln.Decimal.Concrete where

import Penny.Lincoln.Decimal.Normal
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Equivalent
import qualified Deka.Dec as D
import qualified Deka.Native as D
import Prelude hiding (exponent)

newtype Qty = Qty { unQty :: Normal }
  deriving (Eq, Ord, Show)

instance HasCoefficient Qty where
  coefficient = coefficient . unQty

instance HasExponent Qty where
  exponent = exponent . unQty

instance Signed Qty where
  sign = sign . unQty

instance Equivalent Qty where
  compareEv (Qty x) (Qty y) = compareEv x y

newtype Exchange = Exchange { unExchange :: Normal }
  deriving (Eq, Ord, Show)

instance Equivalent Exchange where
  compareEv (Exchange x) (Exchange y) = compareEv x y

instance HasCoefficient Exchange where
  coefficient = coefficient . unExchange

instance HasExponent Exchange where
  exponent = exponent . unExchange

instance Signed Exchange where
  sign = sign . unExchange

qtyLane :: Qty -> Lane Side
qtyLane = genericLane f . unQty
  where
    f s = case s of
      D.Sign0 -> Debit
      D.Sign1 -> Credit

exchLane :: Exchange -> Lane PosNeg
exchLane = genericLane f . unExchange
  where
    f s = case s of
      D.Sign0 -> Pos
      D.Sign1 -> Neg

genericLane :: (D.Sign -> a) -> Normal -> Lane a
genericLane f n = case D.unCoefficient . pmCoefficient $ p of
  D.Nil -> Center
  D.Plenus dc -> NonCenter (f . pmSign $ p, dc)
  where
    p = params n

