-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, unQty, partialNewQty,
  newQty, add, subt, mult,
  zero, Difference(LeftBiggerBy, RightBiggerBy, Equal),
  difference) where

import Data.Decimal ( DecimalRaw ( Decimal ), Decimal )

-- | A quantity is always greater than zero. Various odd questions
-- happen if quantities can be zero. For instance, what if you have a
-- debit whose quantity is zero? Does it require a balancing credit
-- that is also zero? And how can you have a debit of zero anyway?
--
-- I can imagine situations where a quantity of zero might be useful;
-- for instance maybe you want to specifically indicate that a
-- particular posting in a transaction did not happen (for instance,
-- that a paycheck deduction did not take place). I think the better
-- way to handle that though would be through an addition to
-- Debit/Credit - maybe Debit/Credit/Zero. Barring the addition of
-- that, though, the best way to indicate a situation such as this
-- would be through transaction memos.
newtype Qty = Qty Decimal
              deriving (Eq, Ord, Show)

data Difference =
  LeftBiggerBy Qty
  | RightBiggerBy Qty
  | Equal

-- | Subtract the second Qty from the first.
difference :: Qty -> Qty -> Difference
difference (Qty q1) (Qty q2) = case compare q1 q2 of
  GT -> LeftBiggerBy (Qty $ q1 - q2)
  LT -> RightBiggerBy (Qty $ q2 - q1)
  EQ -> Equal

-- | Unwrap a Qty to get the underlying Decimal. This Decimal will
-- always be greater than zero.
unQty :: Qty -> Decimal
unQty (Qty d) = d

-- | Make a new Qty. This function is partial. It will call error if
-- its argument is less than or equal to zero.
partialNewQty :: Decimal -> Qty
partialNewQty d =
  if d <= 0
  then error
       $ "partialNewQty: argument less than or equal to zero: "
       ++ show d
  else Qty d

-- | Make a new Qty. Returns Nothing if its argument is less than
-- zero.
newQty :: Decimal -> Maybe Qty
newQty d = if d <= 0 then Nothing else Just (Qty d)

add :: Qty -> Qty -> Qty
add (Qty q1) (Qty q2) = Qty $ q1 + q2

subt :: Qty -> Qty -> Maybe Qty
subt (Qty q1) (Qty q2) = if q2 > q1 then Nothing else Just $ Qty (q1 - q2)

mult :: Qty -> Qty -> Qty
mult (Qty q1) (Qty q2) = Qty $ q1 * q2

zero :: Qty
zero = Qty $ Decimal 0 0
