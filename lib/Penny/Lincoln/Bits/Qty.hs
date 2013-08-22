{-# LANGUAGE OverloadedStrings #-}
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
  , wholeOrFrac
  , WholeOrFracResult
  , wholeOrFracToQtyRep

  , WholeOnly
  , unWholeOnly
  , wholeOnly

  , WholeOrFrac(..)
  , Radix(..)
  , showRadix
  , QtyRep(..)

  -- ** Converting between quantity representations and quantities
  , qtyToRep
  , qtyToRepNoGrouping
  , qtyToRepGrouped

  -- ** Rendering quantity representations
  , showQtyRep
  , bestRadGroup

  -- * Other stuff
  , Qty
  , HasQty(..)
  , signif
  , places
  , compareQty
  , newQty
  , Signif
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

-- # Imports

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Text (Text)
import qualified Data.Text as X
import Data.Ord(Down(..), comparing)
import Data.List (genericLength, genericReplicate, sortBy, group, sort)
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (NonEmpty((:|)), toList, nonEmpty)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Semigroup(Semigroup(..))
import Data.Semigroup(sconcat)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent (Equivalent(..), (==~))
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

instance Data.Semigroup.Semigroup DigitList where
  (<>) (DigitList l1) (DigitList l2) =
    DigitList $ l1 Data.Semigroup.<> l2

class Digits a where
  digits :: a -> DigitList

instance Digits DigitList where
  digits = id

instance Digits (GroupedDigits a) where
  digits (GroupedDigits d1 dr) = sconcat (d1 :| map snd dr)

-- | All of the digits on a single side of a radix point. Typically
-- this is parameterized on a type that represents the grouping
-- character.
data GroupedDigits a = GroupedDigits
  { dsFirstPart :: DigitList
  -- ^ The first chunk of digits
  , dsNextParts :: [(a, DigitList)]
  -- ^ Optional subsequent chunks of digits. Each is a grouping
  -- character followed by additional digits.
  } deriving (Eq, Show, Ord)


-- | A quantity representation that has both a whole number and a
-- fractional part. Abstract because there must be a non-zero digit in
-- here somewhere, which 'wholeFrac' checks for.  Typically this is
-- parameterized on an instance of the Digits class, such as DigitList
-- or GroupedDigits.  This allows separate types for values that
-- cannot be grouped as well as those that can.
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
-- 'wholeOnly' checks for.  Typically this is parameterized on an
-- instance of the Digits class, such as DigitList or GroupedDigits.
newtype WholeOnly a = WholeOnly { unWholeOnly :: a }
  deriving (Eq, Show, Ord)

wholeOnly :: Digits a => a -> Maybe (WholeOnly a)
wholeOnly d = if digitsHasNonZero d then Just (WholeOnly d) else Nothing

-- | Typically this is parameterized on an instance of the Digits
-- class, such as DigitList or GroupedDigits.
newtype WholeOrFrac a = WholeOrFrac
  { unWholeOrFrac :: Either (WholeOnly a) (WholeFrac a) }
  deriving (Eq, Show, Ord)

wholeOrFrac
  :: GroupedDigits a
  -- ^ What's before the radix point

  -> Maybe (GroupedDigits a)
  -- ^ What's after the radix point (if anything)

  -> Maybe (Either (WholeOrFrac DigitList)
                   (WholeOrFrac (GroupedDigits a)))
wholeOrFrac g@(GroupedDigits l1 lr) mayAft = case mayAft of
  Nothing -> case lr of
    [] -> fmap (Left . WholeOrFrac . Left) $ wholeOnly l1
    _ -> fmap (Right . WholeOrFrac . Left) $ wholeOnly g
  Just aft@(GroupedDigits r1 rr) -> case (lr, rr) of
    ([], []) -> fmap (Left . WholeOrFrac . Right) $ wholeFrac l1 r1
    _ -> fmap (Right . WholeOrFrac . Right) $ wholeFrac g aft


data Radix = Period | Comma
  deriving (Eq, Show, Ord)

type WholeOrFracResult a = Either (WholeOrFrac DigitList)
                                  (WholeOrFrac (GroupedDigits a))

wholeOrFracToQtyRep
  :: Either (WholeOrFracResult PeriodGrp) (WholeOrFracResult CommaGrp)
  -> QtyRep
wholeOrFracToQtyRep e = case e of
  Left p -> case p of
    Left dl -> QNoGrouping dl Period
    Right gd -> QGrouped (Left gd)
  Right c -> case c of
    Left dl -> QNoGrouping dl Comma
    Right gd -> QGrouped (Right gd)

data QtyRep
  = QNoGrouping (WholeOrFrac DigitList) Radix
  | QGrouped (Either (WholeOrFrac (GroupedDigits PeriodGrp))
                     (WholeOrFrac (GroupedDigits CommaGrp)))
  deriving (Eq, Show, Ord)

instance Equivalent QtyRep where
  equivalent x y = toQty x ==~ toQty y
  compareEv x y = Ev.compareEv (toQty x) (toQty y)

-- | Converts an Integer to a list of digits.
intToDigits :: Integer -> NonEmpty Digit
intToDigits
  = fmap intToDigit
  . fromMaybe (error "intToDigits: show made empty list")
  . nonEmpty
  . show
  where
    intToDigit c = case c of
      { '0' -> D0; '1' -> D1; '2' -> D2; '3' -> D3; '4' -> D4;
        '5' -> D5; '6' -> D6; '7' -> D7; '8' -> D8; '9' -> D9;
        _ -> error "intToDigits: show made non-digit character" }

prependNonEmpty :: [a] -> NonEmpty a -> NonEmpty a
prependNonEmpty [] x = x
prependNonEmpty (x:xs) (y1 :| ys) = x :| (xs ++ (y1 : ys))

qtyToRepNoGrouping :: Qty -> WholeOrFrac DigitList
qtyToRepNoGrouping q =
  let sig = intToDigits . signif $ q
      e = places q
      len = genericLength . toList $ sig
  in WholeOrFrac $ if e == 0
     then Left (WholeOnly (DigitList sig))
     else let leadZeroes = genericReplicate (e - len) D0
              w = DigitList $ D0 :| []
              f = DigitList $ prependNonEmpty leadZeroes sig
          in Right (WholeFrac w f)



-- | Given a list of QtyRep, determine the most common radix and
-- grouping that are used.  If a single QtyRep is grouped, then the
-- result is also grouped.  The most common grouping character
-- determines which grouping character is used.
--
-- If no QtyRep are grouped, then the most common radix point is used
-- and the result is not grouped.
--
-- If there is no radix point found, returns Nothing.
bestRadGroup
  :: [QtyRep]
  -> Maybe (S.S3 Radix PeriodGrp CommaGrp)
bestRadGroup ls = fromGrouping <|> fromRadix
  where
    grpToRadix q = case q of
      QNoGrouping _ _ -> Nothing
      QGrouped e -> Just $ either (const Period) (const Comma) e
    mostCommonGrpRad = mode . mapMaybe grpToRadix $ ls
    fromGrouping = do
      rad <- mostCommonGrpRad
      case rad of
        Period -> fmap S.S3b . mostCommonPeriodGrp $ ls
        Comma -> fmap S.S3c . mostCommonCommaGrp $ ls
    fromRadix = fmap S.S3a . mode . mapMaybe noGrpToRadix $ ls
    noGrpToRadix q = case q of
      QNoGrouping _ r -> Just r
      _ -> Nothing

mostCommonPeriodGrp :: [QtyRep] -> Maybe PeriodGrp
mostCommonPeriodGrp
  = mode
  . concatMap f
  where
    f q = case q of
      QNoGrouping _ _ -> []
      QGrouped e -> case e of
        Left (WholeOrFrac ei) -> case ei of
          Left _ -> []
          Right (WholeFrac g1 g2) -> getSeps g1 ++ getSeps g2
        Right _ -> []

mostCommonCommaGrp :: [QtyRep] -> Maybe CommaGrp
mostCommonCommaGrp
  = mode
  . concatMap f
  where
    f q = case q of
      QNoGrouping _ _ -> []
      QGrouped e -> case e of
        Left _ -> []
        Right (WholeOrFrac ei) -> case ei of
          Left _ -> []
          Right (WholeFrac g1 g2) -> getSeps g1 ++ getSeps g2

getSeps :: GroupedDigits a -> [a]
getSeps (GroupedDigits _ ls) = map fst ls


mode :: Ord a => [a] -> Maybe a
mode = listToMaybe . modes

modes :: Ord a => [a] -> [a]
modes
  = map (head . snd)
  . sortBy (comparing (Down . fst))
  . map (\ls -> (length ls, ls))
  . group
  . sort

-- | Groups digits, using the given split character.
groupDigits
  :: NonEmpty a
  -> (NonEmpty a, [NonEmpty a])
  -- ^ The first group of digits, and any subsequent groups.
groupDigits
  = toPair
  . reverse
  . map reverse
  . chunksOf 3
  . reverse
  . toList
  where
    toPair [] = error "groupDigits: chunksOf produced empty list"
    toPair (x:xs) = (ne x, map ne xs)
    ne = fromMaybe (error $ "groupDigits: chunksOf produced"
                            ++ " empty inner list") . nonEmpty

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
qtyToRepGrouped g q = WholeOrFrac
  $ case unWholeOrFrac $ qtyToRepNoGrouping q of
    Left (WholeOnly (DigitList ds)) ->
      Left $ WholeOnly (mkWholeGroups ds)
    Right (WholeFrac w f) ->
      Right $ mkWholeFracGroups (unDigitList w) (unDigitList f)
    where
      mkGroups ds =
        let (g1, gs) = groupDigits ds
            mkGrp dl = (g, DigitList dl)
        in GroupedDigits (DigitList g1) (map mkGrp gs)
      mkWholeGroups ds = if (length . toList $ ds) > maxUngrouped
                         then mkGroups ds
                         else GroupedDigits (DigitList ds) []
      mkWholeFracGroups w f = WholeFrac w' f'
        where
          f' = GroupedDigits (DigitList f) []
          w' = if (length . toList $ w) + (length . toList $ f)
                    > maxUngrouped
               then mkGroups w
               else GroupedDigits (DigitList w) []
      maxUngrouped = 4

qtyToRep
  :: S.S3 Radix PeriodGrp CommaGrp
  -> Qty
  -> QtyRep
qtyToRep x q = case x of
  S.S3a r -> QNoGrouping (qtyToRepNoGrouping q) r
  S.S3b g -> QGrouped . Left $ qtyToRepGrouped g q
  S.S3c g -> QGrouped . Right $ qtyToRepGrouped g q

class HasQty a where
  toQty :: a -> Qty



digitToInt :: Digit -> Integer
digitToInt d = case d of
  { D0 -> 0; D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5;
    D6 -> 6; D7 -> 7; D8 -> 8; D9 -> 9 }

digitsToInt :: DigitList -> Integer
digitsToInt
  = sum
  . map (\(e, s) -> s * 10 ^ e)
  . zip ([0..] :: [Integer])
  . map digitToInt
  . reverse
  . toList
  . unDigitList

instance HasQty QtyRep where
  toQty q = case q of
    QNoGrouping (WholeOrFrac ei) _ -> case ei of
      Left (WholeOnly ds) -> Qty (digitsToInt ds) 0
      Right (WholeFrac w f) -> Qty sig ex
        where
          sig = digitsToInt ((Data.Semigroup.<>) w f)
          ex = genericLength . toList . unDigitList $ f
    QGrouped ei -> either groupedToQty groupedToQty ei

groupedToQty :: WholeOrFrac (GroupedDigits a) -> Qty
groupedToQty (WholeOrFrac ei) = case ei of
  Left (WholeOnly g) -> Qty (digitsToInt . digits $ g) 0
  Right (WholeFrac w f) -> Qty sig ex
    where
      sig = digitsToInt ((Data.Semigroup.<>) (digits w) (digits f))
      ex = genericLength . toList . unDigitList . digits $ f

instance HasQty Qty where
  toQty = id

showDigit :: Digit -> Text
showDigit d = case d of
  { D0 -> "0"; D1 -> "1"; D2 -> "2"; D3 -> "3"; D4 -> "4";
    D5 -> "5"; D6 -> "6"; D7 -> "7"; D8 -> "8"; D9 -> "9" }

showRadix :: Radix -> Text
showRadix r = case r of { Comma -> ","; Period -> "." }

showDigitList :: DigitList -> X.Text
showDigitList = X.concat . toList . fmap showDigit . unDigitList

showGroupedDigits
  :: Grouper a
  => GroupedDigits a
  -> Text
showGroupedDigits (GroupedDigits d ds)
  = showDigitList d <> (X.concat . map f $ ds)
  where
    f (c, cs) = (X.singleton $ groupChar c) <> showDigitList cs

showWholeOnlyDigitList :: WholeOnly DigitList -> Text
showWholeOnlyDigitList = showDigitList . unWholeOnly

showWholeOnlyGroupedDigits
  :: Grouper a
  => WholeOnly (GroupedDigits a)
  -> Text
showWholeOnlyGroupedDigits = showGroupedDigits . unWholeOnly

showWholeFracDigitList
  :: Radix
  -> WholeFrac DigitList
  -> Text
showWholeFracDigitList r wf
  = showDigitList (whole wf) <> showRadix r <> showDigitList (frac wf)

showWholeFracGroupedDigits
  :: Grouper a
  => Radix
  -> WholeFrac (GroupedDigits a)
  -> Text
showWholeFracGroupedDigits r wf
  = showGroupedDigits (whole wf) <> showRadix r
  <> showGroupedDigits (frac wf)

wholeOrFracGrouped
  :: Grouper a
  => Radix
  -> WholeOrFrac (GroupedDigits a)
  -> Text
wholeOrFracGrouped r
  = either showWholeOnlyGroupedDigits (showWholeFracGroupedDigits r)
  . unWholeOrFrac

wholeOrFracDigitList
  :: Radix
  -> WholeOrFrac DigitList
  -> Text
wholeOrFracDigitList r
  = either showWholeOnlyDigitList (showWholeFracDigitList r)
  . unWholeOrFrac


showQtyRep :: QtyRep -> Text
showQtyRep q = case q of
  QNoGrouping wf r -> wholeOrFracDigitList r wf
  QGrouped ei ->
    either (wholeOrFracGrouped Period)
           (wholeOrFracGrouped Comma) ei


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
-- have both the same significand and the same exponent. You may instead
-- want 'equivalent'. Similarly, the Ord instance is derived. It
-- compares based on the integral value of the significand and of the
-- exponent. You may instead want 'compareQty', which compares after
-- equalizing the exponents.
data Qty = Qty { signif :: !Integer
               , places :: !Integer
               } deriving (Eq, Show, Ord)

instance Ev.Equivalent Qty where
  equivalent x y = x' == y'
    where
      (x', y') = equalizeExponents x y
  compareEv x y = compare x' y'
    where
      (x', y') = equalizeExponents x y

type Signif = Integer
type Places = Integer

-- | Significand 1, exponent 0
qtyOne :: Qty
qtyOne = Qty 1 0


newQty :: Signif -> Places -> Maybe Qty
newQty m p
  | m > 0  && p >= 0 = Just $ Qty m p
  | otherwise = Nothing


-- | Compares Qty after equalizing their exponents.
--
-- > compareQty (newQty 15 1) (newQty 1500 3) == EQ
compareQty :: Qty -> Qty -> Ordering
compareQty q1 q2 = compare (signif q1') (signif q2')
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
      (mx, my) = (signif x', signif y')
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
-- Allocate the significands.
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
        multRemainderAllResultsAtLeast1 (signif tot')
        (map signif ls')
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
