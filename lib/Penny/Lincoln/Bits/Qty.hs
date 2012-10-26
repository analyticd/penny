-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, unQty, partialNewQty,
  newQty, add, subt, mult,
  Difference(LeftBiggerBy, RightBiggerBy, Equal),
  difference, allocate) where

import Control.Monad.Loops (iterateUntil)
import Data.Decimal ( DecimalRaw ( Decimal ), Decimal )
import qualified Data.Decimal as D
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import qualified Control.Monad.Trans.State as St
import qualified Data.Traversable as Tr
import Data.Word (Word8)

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
subt (Qty q1) (Qty q2) =
  if q2 > q1
  then Nothing
  else Just $ Qty (q1 - q2)

mult :: Qty -> Qty -> Qty
mult (Qty q1) (Qty q2) = Qty $ q1 * q2

-- | Allocate a Qty proportionally so that the sum of the results adds
-- up to a given Qty. Fails if the allocation cannot be made (e.g. if
-- it is impossible to allocate without overflowing Decimal.) The
-- result will always add up to the given sum.
allocate
  :: Qty
  -- ^ The result will add up to this Qty.

  -> NonEmpty Qty
  -- ^ Allocate using this list of Qty.

  -> Maybe (NonEmpty Qty)
  -- ^ The length of this list will be equal to the length of the list
  -- of allocations. Each item will correspond to the original
  -- allocation. Fails if overflow would result.

allocate (Qty t) =
  fmap (fmap Qty) . allocDecimalsNoZeroes t . fmap unQty


-- | Allocates decimals. If there are no zero decimals in the input,
-- there are no zero decimals in the result. If there are zero
-- decimals in the input, undefined behavior occurs. Fails if overflow
-- would occur.
allocDecimalsNoZeroes
  :: Decimal
  -> NonEmpty Decimal
  -> Maybe (NonEmpty Decimal)
allocDecimalsNoZeroes d ls = St.evalState k 0
  where
    p m = case m of
      Nothing -> True
      Just r -> F.all (> (Decimal 0 0)) r
    k = iterateUntil p (allocDecimalsSt d ls)

-- | Allocates decimals. The state stores the amount to increment the
-- decimal exponents by. This function performs the allocation and
-- then increases the increment.
allocDecimalsSt
  :: Decimal
  -> NonEmpty Decimal
  -> St.State Int (Maybe (NonEmpty Decimal))
allocDecimalsSt d ls = do
  i <- St.get
  St.modify succ
  case allocDecimals i d ls of
    Nothing -> return Nothing
    Just r -> return (Just r)

-- | Allocates decimals. Does not guarantee that the decimals will
-- each be greater than zero.
allocDecimals
  :: Int -> Decimal -> NonEmpty Decimal -> Maybe (NonEmpty Decimal)
allocDecimals i tot ls = do
  (tot', ls', e) <- sameExponent i tot ls
  let totInt = D.decimalMantissa tot'
      lsInt = fmap D.decimalMantissa ls'
      alloced = allocateIntegers totInt lsInt
  return . fmap (Decimal e) $ alloced

-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same mantissa. Fails if overflow would
-- result. Also, you provide an Int for an optional number of
-- additional places to add to the exponent.
sameExponent
  :: Int
  -> Decimal
  -> NonEmpty Decimal
  -> Maybe (Decimal, NonEmpty Decimal, Word8)
sameExponent plus dec ls =
  let maxExp = max (F.maximum . fmap D.decimalPlaces $ ls)
                   (D.decimalPlaces dec)
      newExpInt = plus + fromIntegral maxExp
      newExp8 = fromIntegral plus + maxExp
      conv (Decimal p m) =
        let diff = newExp8 - p
        in Decimal (p + diff) (m * 10 ^ diff)
  in if newExpInt > 255
     then Nothing
     else Just (conv dec, fmap conv ls, newExp8)

-- | Allocate Integers. All input Integers must be positive;
-- otherwise, undefined behavior occurs. All output integers will be
-- at least zero; however, they might be zero (there is no guarantee
-- they will be positive.) All output Integers will add up to the
-- target Integer.
allocateIntegers :: Integer -> NonEmpty Integer -> NonEmpty Integer
allocateIntegers target allocs =
  let totAlloc = toDouble . F.sum $ allocs
      tgtDbl = toDouble target
      getResult alloc = (toDouble alloc * tgtDbl) / totAlloc
      ratios = fmap getResult allocs
      toDouble = fromIntegral :: Integer -> Double
      rounded = fmap truncate ratios
  in addToReachTarget target rounded

-- | Returns a list with amounts added to each element so that its sum
-- is the given value. Spreads out the additions to the extent
-- possible. If the given sum is equal to or less than the sum of the
-- list, returns the list without change.
addToReachTarget
  :: Integer
  -> NonEmpty Integer
  -> NonEmpty Integer
addToReachTarget t ls =
  let tot = F.sum ls
      diff = t - tot
  in if diff < 0
     then ls
     else
      let (addToAll, nToIncrement) =
            diff `divMod` (fromIntegral . length . F.toList $ ls)
      in incrementFirstN nToIncrement . fmap (+ addToAll) $ ls

-- | Increments the first n elements of a list.
incrementFirstN :: Integer -> NonEmpty Integer -> NonEmpty Integer
incrementFirstN i ls = St.evalState (Tr.traverse incr ls) i
  where
    incr e = do
      c <- St.get
      if c > 0
        then St.modify pred >> return (succ e)
        else return e

