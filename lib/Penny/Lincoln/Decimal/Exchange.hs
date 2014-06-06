module Penny.Lincoln.Decimal.Exchange where

import Penny.Lincoln.Decimal.Normal
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Concrete
import Prelude hiding (negate)

-- | Computes a new Qty, after being exchanged.  For example, in
-- @exchange x y@, if @y@ is a 'Qty' that is a 'Debit' of @3@ (which
-- represents, we'll say, 3 oranges) and @x@ is an 'Exchange' that is
-- 'Pos' 5 (which represents that 1 orange is worth 5 dollars) then
-- the result is a 'Debit' of @15@, which represents 15 dollars.
--
-- If the 'Exchange' is 'Neg', then the 'Side' of the result flips:
-- thus, for @y@ of 'Debit' @3@, and @x@ of 'Neg' 5, the result is a
-- 'Credit' of @15@.

exchange :: Exchange -> Qty -> Qty
exchange ea@(Exchange e) (Qty q) = Qty . changeSign $ e * q
  where
    changeSign = case exchLane ea of
      Center -> id
      NonCenter (s, _) -> case s of
        Pos -> id
        Neg -> negate
