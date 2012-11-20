-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty (
  Qty, NumberStr(..), toQty, mantissa, places, newQty,
  Mantissa, Places,
  add, mult, Difference(LeftBiggerBy, RightBiggerBy, Equal),
  equivalent, difference, allocate) where

import qualified Data.Foldable as F
import Data.List (genericLength, genericReplicate, genericSplitAt)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

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


-- | Converts strings to Qty. Fails if any of the strings have
-- non-digits, or if any are negative, or if the result is not greater
-- than zero, or if the strings are empty.
toQty :: NumberStr -> Maybe Qty
toQty ns = case ns of
  Whole s -> fmap (\m -> Qty m 0) (readInteger s)
  WholeRad s -> fmap (\m -> Qty m 0) (readInteger s)
  WholeRadFrac w f -> fromWholeRadFrac w f
  RadFrac f -> fromWholeRadFrac "0" f
  where
    fromWholeRadFrac w f =
      fmap (\m -> Qty m (genericLength f)) (readInteger (w ++ f))

-- | Reads non-negative integers only.
readInteger :: String -> Maybe Integer
readInteger s = case reads s of
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

type Mantissa = Integer
type Places = Integer

newQty :: Mantissa -> Places -> Maybe Qty
newQty m p
  | m > 0  && p >= 0 = Just $ Qty m p
  | otherwise = Nothing

instance Show Qty where
  show (Qty m e) =
    let man = show m
        len = genericLength man
        small = "0." ++ ((genericReplicate (e - len) '0') ++ man)
    in case compare e len of
        GT -> small
        EQ -> small
        LT ->
          let (b, end) = genericSplitAt (len - e) man
          in if e == 0
             then man
             else b ++ ['.'] ++ end


instance Ord Qty where
  compare q1 q2 = compare (mantissa q1') (mantissa q2')
    where
      (q1', q2') = equalizeExponents q1 q2

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
add x y =
  let ((Qty xm e), (Qty ym _)) = equalizeExponents x y
  in Qty (xm + ym) e

mult :: Qty -> Qty -> Qty
mult (Qty xm xe) (Qty ym ye) = Qty (xm * ym) (xe + ye)


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
  let (tot', ls', e') = sameExponent tot ls
      (tI, lsI) = (mantissa tot', fmap mantissa ls')
      (seats, pops, moreE) = growTarget tI lsI
      del = allocateSeats seats pops
      totE = e' + moreE
  in fmap (\m -> Qty m totE) $ del


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


-- | Given an Integer and a list of Integers, multiply all integers by
-- ten raised to an exponent, so that the first Integer is at least as
-- large as the count of the number of Integers in the list. Returns
-- the new Integer, new list of Integers, and the exponent used.
growTarget
  :: Integer
  -> NonEmpty Integer
  -> (Integer, NonEmpty Integer, Integer)
growTarget target is = go target is 0
  where
    len = genericLength . F.toList $ is
    go t xs c =
      let t' = t * 10 ^ c
          xs' = fmap (\x -> x * 10 ^ c) xs
      in if t' >= len
         then (t', xs', c)
         else go t' xs' (c + 1)

type Pop = Integer
type Curr = Integer
type Priority = Double
type Divisor = Double
type Who = Integer
type TotSeats = Integer
type SeatsLeft = Integer
type Delegation = Integer

-- | Calculates a single Huntington-Hill multiplier.
divisor :: Curr -> Divisor
divisor n = let f = fromIntegral n in sqrt (f * (f + 1))

priority :: Pop -> Curr -> Priority
priority p n =
  let fp = fromIntegral p
      fn = fromIntegral n
  in fp / (divisor fn)


allocateSeats
  :: TotSeats
  -> NonEmpty Pop
  -> NonEmpty Delegation
allocateSeats tot pops =
  let start = startingAllocation pops
      r = allocateSeatsR (tot - fromIntegral (M.size start)) start
  in NE.fromList . map snd . M.elems $ r

allocateSeatsR
  :: SeatsLeft
  -> M.Map Who (Pop, Delegation)
  -> M.Map Who (Pop, Delegation)
allocateSeatsR l m =
  let m' = allocateSeat m
  in if l == 0
      then m
      else allocateSeatsR (l - 1) m'


startingAllocation
  :: NonEmpty Pop
  -> M.Map Who (Pop, Delegation)
startingAllocation =
  M.fromList
  . zip [0..]
  . map (\p -> (p, 1))
  . F.toList

allocateSeat
  :: M.Map Who (Pop, Delegation)
  -> M.Map Who (Pop, Delegation)
allocateSeat m =
  let priorities = fmap (\(p, d) -> priority p d) m
      winner = maxValue priorities
  in M.adjust (\(p, d) -> (p, d + 1)) winner m

maxValue :: (Ord k, Ord v) => M.Map k v -> k
maxValue m = case F.foldl' f Nothing . M.assocs $ m of
  Nothing -> error "maxValue error"
  Just (k, _) -> k
  where
    f Nothing (k, v) = Just (k, v)
    f (Just (k1, v1)) (k2, v2) =
      if v1 >= v2 then Just (k1, v1) else Just (k2, v2)

