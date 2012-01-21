{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Penny.Qty (
  Qty, unQty, partialNewQty,
  newQty, add, subt, mult,
  zero, difference) where

import Data.Decimal

newtype Qty = Qty Decimal
              deriving (Eq, Ord, Show)

difference :: Qty -> Qty -> Qty
difference (Qty q1) (Qty q2) = Qty (abs $ q1 - q2)

-- | Unwrap a Qty to get the underlying Decimal. This Decimal will
-- always be at least zero.
unQty :: Qty -> Decimal
unQty (Qty d) = d

-- | Make a new Qty. This function is partial. It will call error if
-- its argument is less than zero.
partialNewQty :: Decimal -> Qty
partialNewQty d =
  if d < 0
  then error $ "partialNewQty: argument less than zero: " ++ show d
  else Qty d

-- | Make a new Qty. Returns Nothing if its argument is less than
-- zero.
newQty :: Decimal -> Maybe Qty
newQty d = if d < 0 then Nothing else Just (Qty d)

add :: Qty -> Qty -> Qty
add (Qty q1) (Qty q2) = Qty $ q1 + q2

subt :: Qty -> Qty -> Maybe Qty
subt (Qty q1) (Qty q2) = if q2 > q1 then Nothing else Just $ Qty (q1 - q2)

mult :: Qty -> Qty -> Qty
mult (Qty q1) (Qty q2) = Qty $ q1 * q2

zero :: Qty
zero = Qty $ Decimal 0 0
