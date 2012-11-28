-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, unQty, partialNewQty,
  newQty, add, subt, mult,
  Difference(LeftBiggerBy, RightBiggerBy, Equal),
  difference, allocate) where

import Control.Monad (when)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Maybe as TM
import Data.Decimal ( DecimalRaw ( Decimal ), Decimal )
import qualified Data.Decimal as D
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
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
  deriving (Eq, Show)

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

allocate t ls =
  fmap (fmap Qty) (allocDecimals (unQty t) (fmap unQty ls))

allocDecimals
  :: D.Decimal -> NonEmpty D.Decimal -> Maybe (NonEmpty D.Decimal)
allocDecimals tot ls = do
  (dt, dls, e) <- sameExponent 0 tot ls
  (r, plus) <- allocNoZeroes (D.decimalMantissa dt)
    (fmap D.decimalMantissa dls)
  Tr.sequenceA
    . fmap (adjustExponent plus)
    . fmap (D.Decimal e) $ r


-- | Given an amount that the exponent was increased by, adjust a
-- Decimal so it is equal to its old value. Fails if there would be
-- overflow.
adjustExponent :: Int -> D.Decimal -> Maybe D.Decimal
adjustExponent maybeNegI (D.Decimal e m) =
  let i = abs maybeNegI
  in if i + (fromIntegral e) > 255
     then Nothing
     else Just $ Decimal (fromIntegral (i + (fromIntegral e))) m


-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same mantissa. Fails if overflow would
-- result. Also, you provide an Int for an optional number of
-- additional places to add to the exponent. The absolute value of
-- this Int is used.
sameExponent
  :: Int
  -> Decimal
  -> NonEmpty Decimal
  -> Maybe (Decimal, NonEmpty Decimal, Word8)
sameExponent maybeNegPlus dec ls =
  let plus = abs maybeNegPlus
      maxExp = max (F.maximum . fmap D.decimalPlaces $ ls)
                   (D.decimalPlaces dec)
      newExpInt = plus + fromIntegral maxExp
      newExp8 = fromIntegral plus + maxExp
      conv (Decimal p m) =
        let diff = newExp8 - p
        in Decimal (p + diff) (m * 10 ^ diff)
  in if newExpInt > 255
     then Nothing
     else Just (conv dec, fmap conv ls, newExp8)

-- | Allocates integers so that no zeroes are in the result. Fails if
-- any of the input is bad. May have had to multiply the integers by a
-- power of ten; this power is returned with the result.
allocNoZeroes
  :: Integer
  -> NonEmpty Integer
  -> Maybe (NonEmpty Integer, Int)
allocNoZeroes tot ls =
  let k = do
        p <- St.get
        St.modify succ
        let tot' = tot * 10 ^ p
        return $ (p, allocIntegers tot' ls)
      pdct (_, r) = isJust r
      loopResult = St.evalState (iterateUntil pdct k) 0
  in case snd loopResult of
    Nothing ->  error "allocNoZeroes: should never happen"
    Just g -> return (g, fst loopResult)


-- | Allocates a non-empty list of integers. Computes an amount for
-- each integer in the list. Stops if any of the allocations would be
-- zero (in such a case, increase the size of what to allocate, then
-- try again.)
allocIntegers
  :: Integer
  -- ^ All the results will sum up to this integer.

  -> NonEmpty Integer
  -- ^ Allocate these integers.

  -> Maybe (NonEmpty Integer)

allocIntegers tgt ls =
  let sumAllocs = F.sum ls
      nAllocs = length . F.toList $ ls
      lsWithIxs = NE.zip (NE.iterate succ 0) ls
      k = Tr.traverse (allocator sumAllocs tgt nAllocs) lsWithIxs
  in St.evalState (TM.runMaybeT k) tgt

type AllocLeft = Integer

-- | Stateful computation that allocates integers.
allocator
  :: Integer
  -- ^ Sum of all allocations

  -> Integer
  -- ^ Target total

  -> Int
  -- ^ Total number of all allocations (that's the number of
  -- allocations, not their sum)

  -> (Int, Integer)
  -- ^ This allocation's index and the allocation

  -> TM.MaybeT (St.State AllocLeft) Integer
allocator s t n (ix, a) = do
  when (s < 1 || t < 1 || n < 1 || a < 1)
    $ error "allocator: fail 1"
  when ((ix > n - 1) || (ix < 0)) $ error "allocator: fail 2"
  let td = fromIntegral :: Integer -> Double
      r = round (((td a) * (td t)) / (td s))
  if ix == (n - 1)
    then do
      l <- lift St.get
      when (l < 1) (fail "")
      lift $ St.put 0
      return l
    else do
      when (r < 1) (fail "")
      lift $ St.modify (subtract r)
      return r
