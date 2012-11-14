-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, mantissa, places, add, mult,
  Difference(LeftBiggerBy, RightBiggerBy, Equal),
  difference, allocate) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Maybe as TM
import qualified Data.Foldable as F
import Data.List (genericLength, genericReplicate, genericSplitAt)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Control.Monad.Trans.State as St
import qualified Data.Traversable as Tr
import Data.Word (Word8)

data NumberStr =
  Whole String
  -- ^ A whole number only. No radix point.
  | WholeRad String
    -- ^ A whole number and a radix point, but nothing after the radix
    -- point.
  | WholeRadFrac String String
    -- ^ A whole number and something after the radix point.
  | RadFrac String
    -- ^ A radix point and a fractional value after it, but nothing
    -- before the radix point.
  deriving Show


-- | Do not use Prelude.read or Prelude.reads on whole decimal strings
-- like @232.72@. Sometimes it will fail, though sometimes it will
-- succeed; why is not clear to me. Hopefully reading integers won't
-- fail! However, in case it does, use read', whose error message will
-- at least tell you what number was being read.
toDecimal :: NumberStr -> Maybe Qty
toDecimal ns = case ns of
  Whole s -> fmap (\m -> Qty m 0) (readInteger s)
  WholeRad s -> fmap (\m -> Qty m 0) (readInteger s)
  WholeRadFrac w f -> fromWholeRadFrac w f
  RadFrac f -> fromWholeRadFrac "0" f
  where
    fromWholeRadFrac w f =
      let len = length f
      in Just 
      in if len > 255
         then Nothing
         else Just $ D.Decimal (fromIntegral len) (readWithErr (w ++ f))

-- | Reads non-negative integers only.
readInteger :: String -> Maybe Integer
readInteger s = reads s of
  (i, ""):[] -> if i < 0 then Nothing else Just i
  _ -> Nothing

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
--
-- The Eq instance is derived. Therefore q1 == q2 only if q1 and q2
-- have both the same mantissa and the same exponent. You may instead
-- want 'equivalent'.
data Qty = Qty { mantissa :: Integer
               , places :: Integer
               } deriving Eq

instance Show Qty where
  show (Qty m e) =
    let man = show m
        len = genericLength man
    in case compare e len of
        GT -> '.' : ((genericReplicate (e - len) '0') ++ man)
        _ ->
          let (b, end) = genericSplitAt (len - e) man
          in b ++ ['.'] ++ end

-- | Adjust the exponents on two Qty so they are equivalent
-- before, but now have the same exponent.
equalizeExponents :: Qty -> Qty -> (Qty, Qty)
equalizeExponents x y = (x', y')
  where
    (ex, ey) = (places x, places y)
    (x', y') = case compare ex ey of
      GT -> (x, increaseExponent (ex - ey) y)
      LT -> (increaseExponent (ey - ex) x, y)
      EQ -> (x, y)

-- | Increase the exponent by the amount given, so that the new Qty is
-- equivalent to the old one. Takes the absolute value of the
-- adjustment argument.
increaseExponent :: Integer -> Qty -> Qty
increaseExponent i (Qty m e) = Qty m' e'
  where
    amt = abs i
    m' = m * 10 ^ amt
    e' = e + amt

-- | Increases the exponent to the given amount. Does nothing if the
-- exponent is already at or higher than this amount.
increaseExponentTo :: Integer -> Qty -> Qty
increaseExponentTo i q@(Qty _ e) =
  let diff = i - e
  in if diff >= 0 then increaseExponent diff q else q

-- | Compares Qty after equalizing their exponents.
equivalent :: Qty -> Qty -> Bool
equivalent x y = x' == y'
  where
    (x', y') = equalizeExponents x y

data Difference =
  LeftBiggerBy Qty
  | RightBiggerBy Qty
  | Equal
  deriving (Eq, Show)

-- | Subtract the second Qty from the first, after equalizing their
-- exponents.
difference :: Qty -> Qty -> Difference
difference x y =
  let (x', y') = equalizeExponents x y
      (mx, my) = (mantissa x', mantissa y')
  in case compare mx my of
    GT -> LeftBiggerBy (Qty (mx - my) (places x'))
    LT -> RightBiggerBy (Qty (my - mx) (places x'))
    EQ -> Equal

add :: Qty -> Qty -> Qty
add (Qty xm xe) (Qty ym _) = Qty (xm + ym) xe

mult :: Qty -> Qty -> Qty
mult (Qty xm xe) (Qty ym ye) = Qty (xm * ym) (xe * ye)


-- | Allocate a Qty proportionally so that the sum of the results adds
-- up to a given Qty. Fails if the allocation cannot be made (e.g. if
-- it is impossible to allocate without overflowing Decimal.) The
-- result will always add up to the given sum.
allocate
  :: Qty
  -- ^ The result will add up to this Qty.

  -> NonEmpty Qty
  -- ^ Allocate using this list of Qty.

  -> NonEmpty Qty
  -- ^ The length of this list will be equal to the length of the list
  -- of allocations. Each item will correspond to the original
  -- allocation.

allocate tot ls =
  let (tot', ls', newExp) = sameExponent tot ls
      (r, plus) = allocNoZeroes (mantissa tot') (fmap mantissa ls')
      allocedDecs = fmap (\i -> Qty i newExp) r
  in fmap (increaseExponent plus) allocedDecs


-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same exponent. Also returns new exponent.
sameExponent
  :: Qty
  -> NonEmpty Qty
  -> (Qty, NonEmpty Qty, Integer)
sameExponent dec ls =
  let newExp = max (F.maximum . fmap places $ ls)
                   (places dec)
      dec' = increaseExponentTo newExp dec
      ls' = fmap (increaseExponentTo newExp) ls
  in (dec', ls', newExp)

-- | Allocates integers so that no zeroes are in the result.
-- May have had to multiply the integers by a
-- power of ten; this power is returned with the result.
allocNoZeroes
  :: Integer
  -> NonEmpty Integer
  -> (NonEmpty Integer, Integer)
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
    Just g -> (g, fst loopResult)


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
