-- | The 'Concrete' data type and associated functions.  These
-- are concrete numbers, together with a 'Lane' (that is, whether it
-- is a debit, credit, or zero.)  These numbers cannot be rendered
-- as strings; for that, you will need to convert them to a 'Rep'.
-- However, 'Concrete' types are the only ones with which you can
-- perform arithmetic.
module Penny.Lincoln.Decimal.Concrete where

import Penny.Lincoln.Decimal.Normal
import Penny.Lincoln.Decimal.Components
import qualified Deka.Dec as D
import qualified Deka.Native as D

newtype Qty = Qty { unQty :: Normal }
  deriving (Eq, Ord, Show)

newtype Exchange = Exchange { unExchange :: Normal }
  deriving (Eq, Ord, Show)

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
  Nil -> Center
  D.Plenus dc -> NonCenter (f . pmSign $ p, dc)
  where
    p = params n

