{-# LANGUAGE DeriveGeneric #-}
-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty
  (
  -- * Quantity representations
  -- ** Components of quantity representations
    Digit(..)
  , DigitList(..)
  , Digits(..)
  , Grouper(..)
  , PeriodGrp(..)
  , CommaGrp(..)
  , GroupedDigits(..)

  , WholeFrac
  , whole
  , frac
  , wholeFrac

  , WholeOnly
  , unWholeOnly
  , wholeOnly

  , WholeOrFrac(..)
  , Radix(..)
  , QtyRep(..)

  -- ** Converting between quantity representations and quantities
  , repToQty
  , qtyToRep
  , qtyToRepNoGrouping
  , qtyToRepGrouped

  -- ** Rendering quantity representations
  , renderRep
  , renderGrouped
  , renderNoGrouping

  -- * Other stuff
  , Qty
  , NumberStr(..)
  , toQty
  , mantissa
  , places
  , prettyShowQty
  , compareQty
  , newQty
  , Mantissa
  , Places
  , add
  , mult
  , Difference(LeftBiggerBy, RightBiggerBy, Equal)
  , difference
  , allocate
  , TotSeats
  , PartyVotes
  , SeatsWon
  , largestRemainderMethod
  , qtyOne
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Binary as B
import GHC.Generics (Generic)
import Data.List (genericLength, genericReplicate, genericSplitAt, sortBy)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Ord (comparing)
import qualified Penny.Lincoln.Equivalent as Ev
import qualified Penny.Steel.Sums as S

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The digit grouping character when the radix is a period.
data PeriodGrp
  = PGSpace
  -- ^ ASCII space
  | PGThinSpace
  -- ^ Unicode code point 0x2009
  | PGComma
  -- ^ Comma
  deriving (Eq, Show, Ord)

-- | The digit grouping character when the radix is a comma.
data CommaGrp
  = CGSpace
  -- ^ ASCII space
  | CGThinSpace
  -- ^ Unicode code point 0x2009
  | CGPeriod
  -- ^ Period
  deriving (Eq, Show, Ord)

class Grouper a where
  groupChar :: a -> Char

instance Grouper PeriodGrp where
  groupChar c = case c of
    PGSpace -> ' '
    PGThinSpace -> '\x2009'
    PGComma -> ','

instance Grouper CommaGrp where
  groupChar c = case c of
    CGSpace -> ' '
    CGThinSpace -> '\x2009'
    CGPeriod -> '.'

newtype DigitList = DigitList { unDigitList :: NonEmpty Digit }
  deriving (Eq, Show, Ord)

class Digits a where
  digits :: a -> DigitList

-- | All of the digits on a single side of a radix point.
data GroupedDigits a = GroupedDigits
  { dsFirstPart :: DigitList
  -- ^ The first chunk of digits
  , dsNextParts :: [(a, DigitList)]
  -- ^ Optional subsequent chunks of digits. Each is a grouping
  -- character followed by additional digits.
  } deriving (Eq, Show, Ord)


-- | A quantity representation that has both a whole number and a
-- fractional part. Abstract because there must be a non-zero digit in
-- here somewhere, which 'wholeFrac' checks for.
data WholeFrac a = WholeFrac
  { whole :: a
  , frac :: a
  } deriving (Eq, Show, Ord)

wholeFrac
  :: Digits a
  => a
  -- ^ Whole part
  -> a
  -- ^ Fractional part
  -> Maybe (WholeFrac a)
  -- ^ If there is no non-zero digit present, Nothing. Otherwise,
  -- returns the appropriate WholeFrac.
wholeFrac w f = if digitsHasNonZero w || digitsHasNonZero f
  then (Just (WholeFrac w f)) else Nothing

digitsHasNonZero :: Digits a => a -> Bool
digitsHasNonZero = any (/= D0) . toList . unDigitList . digits

-- | A quantity representation that has a whole part only. Abstract
-- because there must be a non-zero digit in here somewhere, which
-- 'wholeOnly' checks for.
newtype WholeOnly a = WholeOnly { unWholeOnly :: a }
  deriving (Eq, Show, Ord)

wholeOnly :: Digits a => a -> Maybe (WholeOnly a)
wholeOnly d = if digitsHasNonZero d then Just (WholeOnly d) else Nothing

newtype WholeOrFrac a = WholeOrFrac
  { unWholeOrFrac :: Either (WholeOnly a) (WholeFrac a) }
  deriving (Eq, Show, Ord)

data Radix = Period | Comma
  deriving (Eq, Show, Ord)

data QtyRep
  = QNoGrouping (WholeOrFrac DigitList) Radix
  | QGrouped (Either (WholeOrFrac (GroupedDigits PeriodGrp))
                     (WholeOrFrac (GroupedDigits CommaGrp)))
  deriving (Eq, Show, Ord)

qtyToRepNoGrouping :: Qty -> WholeOrFrac DigitList
qtyToRepNoGrouping = undefined


-- Digit grouping.  Here are the rules.
--
-- No digits to the right of the decimal point are ever grouped.  For
-- now I will consider this a rare enough case that I will not bother
-- with it.
--
-- Digits to the left of the decimal point are grouped as follows:
--
-- No grouping is performed unless the entire number (including the
-- fractional portion) is at least five digits long.  That means that
-- 1234.5 is grouped into 1,234.5 but 1234 is not grouped.
--
-- Grouping is performed on the whole part only, and digits are
-- grouped every third place.

qtyToRepGrouped :: g -> Qty -> WholeOrFrac (GroupedDigits g)
qtyToRepGrouped = undefined

qtyToRep
  :: S.S3 Radix PeriodGrp CommaGrp
  -> Qty
  -> QtyRep
qtyToRep = undefined

-- | Converts a quantity representation to a quantity.
repToQty :: QtyRep -> Qty
repToQty = undefined

renderRep :: QtyRep -> String
renderRep = undefined

renderGrouped
  :: Grouper g
  => WholeOrFrac (GroupedDigits g)
  -> String
renderGrouped = undefined

renderNoGrouping
  :: Radix
  -> WholeOrFrac DigitList
  -> String
renderNoGrouping = undefined

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
-- Debit\/Credit - maybe Debit\/Credit\/Zero. Barring the addition of
-- that, though, the best way to indicate a situation such as this
-- would be through transaction memos.
--
-- /WARNING/ - before doing comparisons or equality tests
--
-- The Eq instance is derived. Therefore q1 == q2 only if q1 and q2
-- have both the same mantissa and the same exponent. You may instead
-- want 'equivalent'. Similarly, the Ord instance is derived. It
-- compares based on the integral value of the mantissa and of the
-- exponent. You may instead want 'compareQty', which compares after
-- equalizing the exponents.
data Qty = Qty { mantissa :: !Integer
               , places :: !Integer
               } deriving (Eq, Show, Ord, Generic)

-- | Shows a quantity, nicely formatted after accounting for both the
-- mantissa and decimal places, e.g. @0.232@ or @232.12@ or whatever.
prettyShowQty :: Qty -> String
prettyShowQty q =
  let man = show . mantissa $ q
      e = places q
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

instance Ev.Equivalent Qty where
  equivalent x y = x' == y'
    where
      (x', y') = equalizeExponents x y
  compareEv x y = compare x' y'
    where
      (x', y') = equalizeExponents x y

instance B.Binary Qty

type Mantissa = Integer
type Places = Integer

-- | Mantissa 1, exponent 0
qtyOne :: Qty
qtyOne = Qty 1 0




newQty :: Mantissa -> Places -> Maybe Qty
newQty m p
  | m > 0  && p >= 0 = Just $ Qty m p
  | otherwise = Nothing



-- | Compares Qty after equalizing their exponents.
--
-- > compareQty (newQty 15 1) (newQty 1500 3) == EQ
compareQty :: Qty -> Qty -> Ordering
compareQty q1 q2 = compare (mantissa q1') (mantissa q2')
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


--
-- Allocation
--
-- The steps of allocation:
--
-- Adjust all exponents, both on the amount to be allocated and on all
-- the votes, so that the exponents are all equal.
--
-- Allocate the mantissas.
--
-- Return the quantities with the original exponents.

-- | Allocate a Qty proportionally so that the sum of the results adds
-- up to a given Qty. Fails if the allocation cannot be made (e.g. if
-- it is impossible to allocate without overflowing Decimal.) The
-- result will always add up to the given sum.
allocate :: Qty -> (Qty, [Qty]) -> (Qty, [Qty])
allocate tot (q1, qs) = case allocate' tot (q1:qs) of
  [] -> error "allocate error"
  x:xs -> (x, xs)

allocate'
  :: Qty
  -- ^ The result will add up to this Qty.

  -> [Qty]
  -- ^ Allocate using these Qty (there must be at least one).

  -> [Qty]
  -- ^ The length of this list will be equal to the length of the list
  -- of allocations. Each item will correspond to the original
  -- allocation.

allocate' tot ls =
  let ((tot':ls'), e) = sameExponent (tot:ls)
      (moreE, (_, ss)) =
        multRemainderAllResultsAtLeast1 (mantissa tot')
        (map mantissa ls')
      totE = e + moreE
  in map (\m -> Qty m totE) ss



-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same exponent. Also returns new exponent.
sameExponent
  :: [Qty]
  -> ([Qty], Integer)
sameExponent ls =
  let newExp = maximum . fmap places $ ls
  in (map (increaseExponentTo newExp) ls, newExp)





type Multiplier = Integer

multLargestRemainder
  :: TotSeats
  -> [PartyVotes]
  -> Multiplier
  -> (TotSeats, [SeatsWon])
multLargestRemainder ts pv m =
  let ts' = ts * 10 ^ m
      pv' = map (\x -> x * 10 ^ m) pv
  in (ts', largestRemainderMethod ts' pv')

increasingMultRemainder
  :: TotSeats
  -> [PartyVotes]
  -> [(Multiplier, (TotSeats, [SeatsWon]))]
increasingMultRemainder ts pv =
  zip [0..] (map (multLargestRemainder ts pv) [0..])

multRemainderAllResultsAtLeast1
  :: TotSeats
  -> [PartyVotes]
  -> (Multiplier, (TotSeats, [SeatsWon]))
multRemainderAllResultsAtLeast1 ts pv
  = head
  . dropWhile (any (< 1) . snd . snd)
  $ increasingMultRemainder ts pv

-- Largest remainder method: votes for one party is divided by
-- (total votes / number of seats). Result is an integer and a
-- remainder. Each party gets the number of seats indicated by its
-- integer. Parties are then ranked on the basis of the remainders, and
-- those with the largest remainders get an additional seat until all
-- seats have been distributed.
type AutoSeats = Integer
type PartyVotes = Integer
type TotVotes = Integer
type TotSeats = Integer
type Remainder = Rational
type SeatsWon = Integer

-- | Allocates integers using the largest remainder method. This is
-- the method used to allocate parliamentary seats in many countries,
-- so the types are named accordingly.
largestRemainderMethod
  :: TotSeats
  -- ^ Total number of seats in the legislature. This is the integer
  -- that will be allocated. This number must be positive or this
  -- function will fail at runtime.

  -> [PartyVotes]
  -- ^ The total seats will be allocated proportionally depending on
  -- how many votes each party received. The sum of this list must be
  -- positive, and each member of the list must be at least zero;
  -- otherwise a runtime error will occur.

  -> [SeatsWon]
  -- ^ The sum of this list will always be equal to the total number
  -- of seats, and its length will always be equal to length of the
  -- PartyVotes list.

largestRemainderMethod ts pvs =
  let err s = error $ "largestRemainderMethod: error: " ++ s
  in Ex.resolve err $ do
    Ex.assert "TotalSeats not positive" (ts > 0)
    Ex.assert "sum of [PartyVotes] not positive" (sum pvs > 0)
    Ex.assert "negative member of [PartyVotes]" (minimum pvs >= 0)
    return (allocRemainder ts . allocAuto ts $ pvs)


autoAndRemainder
  :: TotSeats -> TotVotes -> PartyVotes -> (AutoSeats, Remainder)
autoAndRemainder ts tv pv =
  let fI = fromIntegral :: Integer -> Rational
      quota = if ts == 0
              then error "autoAndRemainder: zero total seats"
              else if tv == 0
                   then error "autoAndRemainder: zero total votes"
                   else fI tv / fI ts
  in properFraction (fI pv / quota)


allocAuto :: TotSeats -> [PartyVotes] -> [(AutoSeats, Remainder)]
allocAuto ts pvs = map (autoAndRemainder ts (sum pvs)) pvs

allocRemainder
  :: TotSeats
  -> [(AutoSeats, Remainder)]
  -> [SeatsWon]
allocRemainder ts ls =
  let totLeft = ts - (sum . map fst $ ls)
      (leftForEach, stillLeft) = totLeft `divMod` genericLength ls
      wIndex = zip ([0..] :: [Integer]) ls
      sorted = sortBy (comparing (snd . snd)) wIndex
      wOrder = zip [0..] sorted
      awarder (ord, (ix, (as, _))) =
        if ord < stillLeft
        then (ix, as + leftForEach + 1)
        else (ix, as + leftForEach)
      awarded = map awarder wOrder
  in map snd . sortBy (comparing fst) $ awarded
