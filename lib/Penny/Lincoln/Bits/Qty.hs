-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, unQty, partialNewQty,
  newQty, add, subt, mult,
  Difference(LeftBiggerBy, RightBiggerBy, Equal),
  difference, allocate) where

import Control.Monad.Exception.Synchronous as Ex
import Control.Monad (when)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Trans.Class (lift)
import Data.Decimal ( DecimalRaw ( Decimal ), Decimal )
import qualified Data.Decimal as D
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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

allocate (Qty t) = undefined

data IntAllocResult =
  AllocGood (NonEmpty Integer)
  | InvalidInput
  | NonPositiveInInput
  | ZeroInResult
  deriving (Show, Eq)

-- | Allocates a non-empty list of integers. Computes an amount for
-- each integer in the list. Stops if any of the allocations would be
-- zero (in such a case, increase the size of what to allocate, then
-- try again.)
allocIntegers
  :: Integer
  -- ^ All the results will sum up to this integer.

  -> NonEmpty Integer
  -- ^ Allocate these integers.

  -> IntAllocResult

allocIntegers tgt ls =
  let sumAllocs = F.sum ls
      nAllocs = length . F.toList $ ls
      lsWithIxs = NE.zip (NE.iterate succ 0) ls
      k = Tr.traverse (allocator sumAllocs tgt nAllocs) lsWithIxs
  in case St.evalState (Ex.runExceptionalT k) tgt of
      Ex.Exception e -> case e of
        AFNonPositiveInInput -> NonPositiveInInput
        AFInvalidInput -> InvalidInput
        AFZeroInResult -> ZeroInResult
      Ex.Success g -> AllocGood g

data AllocatorFail =
  AFNonPositiveInInput | AFZeroInResult | AFInvalidInput
  deriving (Show, Eq)

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

  -> Ex.ExceptionalT AllocatorFail (St.State AllocLeft) Integer
allocator s t n (ix, a) = do
  when (s < 1 || t < 1 || n < 1 || a < 1)
    $ Ex.throwT AFNonPositiveInInput
  when ((ix > n - 1) || (ix < 0)) $ Ex.throwT AFInvalidInput
  let td = fromIntegral :: Integer -> Double
      r = truncate (((td a) * (td t)) / (td s))
  if ix == (n - 1)
    then do
      l <- lift St.get
      lift $ St.put 0
      return l
    else do
      when (r < 1) (Ex.throwT AFZeroInResult)
      lift $ St.modify (subtract r)
      return r
