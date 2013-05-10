{-# LANGUAGE CPP, DeriveGeneric #-}
-- | Penny quantities. A quantity is simply a count (possibly
-- fractional) of something. It does not have a commodity or a
-- Debit/Credit.
module Penny.Lincoln.Bits.Qty
  ( Qty
  , NumberStr(..)
  , toQty
  , mantissa
  , places
  , newQty
  , Mantissa
  , Places
  , add
  , mult
  , Difference(LeftBiggerBy, RightBiggerBy, Equal)
  , equivalent
  , difference
  , allocate
  , TotSeats
  , PartyVotes
  , SeatsWon
  , largestRemainderMethod
  , qtyOne

#ifdef test
  , tests
  , genSized
  , genRangeInt
  , genMutate
  , genEquivalent
  , genBalQtys
#endif

  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Binary as B
import GHC.Generics (Generic)
import Data.List (genericLength, genericReplicate, genericSplitAt, sortBy)
import Data.Ord (comparing)

#ifdef test
import Control.Monad (liftM2)
import Data.List (foldl1')
import Data.Maybe (isJust, isNothing)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck (Arbitrary(..), Gen, (==>))
import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: TF.Test
tests = TF.testGroup "Penny.Lincoln.Bits.Qty"
  [ testProperty "mantissa is greater than zero" prop_mantissa
  , testProperty "exponent is always at least zero" prop_exponent
  , testProperty "newQty succeeds" prop_newQtySucceeds
  , testProperty "add is commutative"
    $ Q.mapSize (min 10) prop_commutative

  , testProperty "equalizeExponents yields equal exponents"
    $ Q.mapSize (min 10) prop_equalExponents

  , testProperty "equalizeExponents gives valid Qtys"
    $ Q.mapSize (min 10) prop_validEqualExponents

  , testProperty "genBalQtys generates valid Qty"
    $ Q.mapSize (min 10) prop_genBalQtysValid

  , testProperty "add generates valid Qty"
    $ Q.mapSize (min 10) prop_addValid

  , testProperty "mult generates valid Qty"
    $ Q.mapSize (min 10) prop_multValid

  , testProperty "genOne generates valid Qty"
    $ Q.mapSize (min 10) prop_genOneValid

  , testProperty "identity of multiplication"
    $ Q.mapSize (min 10) prop_multIdentity

  , testProperty "newQty fails on bad mantissa"
    $ Q.mapSize (min 10) prop_newQtyBadMantissa

  , testProperty "newQty fails on bad exponent"
    $ Q.mapSize (min 10) prop_newQtyBadPlaces

  , testProperty "difference generates valid Qty"
    $ Q.mapSize (min 10) prop_differenceValid

  , testProperty "allocate creates valid Qty"
    $ Q.mapSize (min 10) prop_allocateValid

  , testProperty "genEquivalent generates equivalent Qty"
    $ Q.mapSize (min 10) prop_genEquivalent

  , testProperty "equivalent fails on different Qty"
    $ Q.mapSize (min 10) prop_genNotEquivalent

  , testProperty "genEquivalent generates valid Qty"
    $ Q.mapSize (min 10) prop_genEquivalentValid

  , testProperty "genMutate generates valid Qty"
    $ Q.mapSize (min 10) prop_genMutateValid

  , testProperty "prop_addSubtract"
    $ Q.mapSize (min 10) prop_addSubtract

  , testProperty "genBalQtys generates balanced groups of quantities"
    $ Q.mapSize (min 10) prop_genBalQtys

  , testProperty ( "genBalQtys generates first group of quantities "
                   ++  " with correct sum")
    $ Q.mapSize (min 10) prop_genBalQtysTotalX

  , testProperty "Sum of allocation adds up to original Qty"
    $ Q.mapSize (min 10) prop_sumAllocate

  , testProperty "prop_sameExponent: sameExponent gives same exponent"
    $ Q.mapSize (min 10) prop_sameExponent

  , testProperty "Number of allocations is same as number requested"
    $ Q.mapSize (min 10) prop_numAllocate

  , testProperty "Sum of largest remainder method is equal to total"
    prop_sumLargestRemainder
  ]
#endif

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
data Qty = Qty { mantissa :: !Integer
               , places :: !Integer
               } deriving (Eq, Generic)

instance B.Binary Qty

type Mantissa = Integer
type Places = Integer

-- | Mantissa 1, exponent 0
qtyOne :: Qty
qtyOne = Qty 1 0

#ifdef test

-- | Generates Qty where the mantissa and the exponent depend on the
-- size parameter.
genSized :: Gen Qty
genSized = do
  m <- Q.suchThat Q.arbitrarySizedIntegral (> (0 :: Integer))
  p <- Q.suchThat Q.arbitrarySizedIntegral (>= (0 :: Integer))
  return $ Qty m p

-- | Generates Qty where the mantissa and exponent are over the range
-- of Int, but small mantissas and exponents are generated more than
-- large ones.
genRangeInt :: Gen Qty
genRangeInt = do
  m <- Q.suchThat Q.arbitrarySizedBoundedIntegral (> (0 :: Int))
  p <- Q.suchThat Q.arbitrarySizedBoundedIntegral (>= (0 :: Int))
  return $ Qty (fromIntegral m) (fromIntegral p)

-- | Generates Qty with small exponents.
genSmallExp :: Gen Qty
genSmallExp = liftM2 Qty
  (fmap fromIntegral $ Q.suchThat Q.arbitrarySizedBoundedIntegral
                                  (> (0 :: Int)))
  (fmap fromIntegral $ Q.choose (0, 4 :: Int))

-- | Mutates a Qty so that it is equivalent, but possibly with a
-- different mantissa and exponent.
genEquivalent :: Qty -> Gen Qty
genEquivalent (Qty m p) = do
  expo <- Q.suchThat Q.arbitrarySizedBoundedIntegral (>= (0 :: Int))
  let m' = m * (10 ^ expo)
      p' = p + (fromIntegral expo)
  return $ Qty m' p'

-- | Mutates a Qty so that it is not equivalent. Changes either the
-- mantissa or the exponent or both.
genMutate :: Qty -> Gen Qty
genMutate (Qty m p) = do
  (changeMantissa, changeExp) <-
    Q.suchThat (liftM2 (,) arbitrary arbitrary)
    (/= (False, False))
  m' <- if changeMantissa then mutateAtLeast1 m else return m
  p' <- if changeExp then mutateAtLeast0 p else return p
  return $ Qty m' p'

-- | Mutates an Integer.  The result is always at least one.
mutateAtLeast1 :: Integer -> Gen Integer
mutateAtLeast1 i =
  fmap fromIntegral $ Q.suchThat Q.arbitrarySizedBoundedIntegral pdct
  where
    pdct = if i > (fromIntegral (maxBound :: Int))
              || i < (fromIntegral (minBound :: Int))
           then (>= (1 :: Int))
           else (\r -> r >= 1 && r /= (fromIntegral i))

-- | Mutates an Integer. The result is always at least zero.
mutateAtLeast0 :: Integer -> Gen Integer
mutateAtLeast0 i =
  fmap fromIntegral $ Q.suchThat Q.arbitrarySizedBoundedIntegral pdct
  where
    pdct = if i > (fromIntegral (maxBound :: Int))
              || i < (fromIntegral (minBound :: Int))
           then (>= (0 :: Int))
           else (\r -> r >= 0 && r /= (fromIntegral i))

-- | Generates one, with different exponents.
genOne :: Gen Qty
genOne = do
  p <- fmap fromIntegral $ Q.choose (0, 100 :: Int)
  return (Qty (1 * 10 ^ p) p)

newtype One = One { unOne :: Qty }
  deriving (Eq, Show)

instance Arbitrary One where arbitrary = fmap One genOne

-- | Chooses one of 'genSized' or 'genRangeInt' or 'genSmallExp'.
instance Arbitrary Qty where
  arbitrary = Q.oneof [ genSized
                      , genRangeInt, genSmallExp ]

-- | Mantissas are always greater than zero.
prop_mantissa :: Qty -> Bool
prop_mantissa q = mantissa q > 0

-- | Exponent is always at least zero
prop_exponent :: Qty -> Bool
prop_exponent q = places q >= 0

-- | newQty passes if exponent is at least zero and if mantissa is
-- greater than zero.

prop_newQtySucceeds :: Mantissa -> Places -> Q.Property
prop_newQtySucceeds m p =
  m > 0 ==> p >= 0 ==> isJust (newQty m p)

-- | True if this is a valid Qty; that is, the mantissa is greater
-- than 0 and the number of places is greater than or equal to 0.
validQty :: Qty -> Bool
validQty (Qty m p) = m > 0 && p >= 0

#endif

newQty :: Mantissa -> Places -> Maybe Qty
newQty m p
  | m > 0  && p >= 0 = Just $ Qty m p
  | otherwise = Nothing

-- | Shows a quantity, nicely formatted after accounting for both the
-- mantissa and decimal places, e.g. @0.232@ or @232.12@ or whatever.
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


-- | Compares Qty after equalizing their exponents.
--
-- > compare (newQty 15 1) (newQty 1500 3) == EQ
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

#ifdef test

-- | equalizeExponents gives two Qty with equal exponents.

prop_equalExponents :: Qty -> Qty -> Bool
prop_equalExponents q1 q2 =
  let (q1', q2') = equalizeExponents q1 q2
  in places q1' == places q2'

-- | equalizeExponents gives valid Qtys
prop_validEqualExponents :: Qty -> Qty -> Bool
prop_validEqualExponents q1 q2 =
  let (q1', q2') = equalizeExponents q1 q2
  in validQty q1' && validQty q2'

#endif

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

#ifdef test

-- | Generates a group of balanced quantities.
genBalQtys :: Gen (Qty, [Qty], [Qty])
genBalQtys = do
  total <- arbitrary
  group1alloc1 <- arbitrary
  group1allocRest <- arbitrary
  group2alloc1 <- arbitrary
  group2allocRest <- arbitrary
  let (g1r1, g1rs) = allocate total (group1alloc1, group1allocRest)
      (g2r1, g2rs) = allocate total (group2alloc1, group2allocRest)
  return $ (total, g1r1 : g1rs, g2r1 : g2rs)

showQty :: Qty -> String
showQty q = "mantissa: " ++ show (mantissa q) ++ " exponent: "
            ++ show (places q)

-- | genBalQtys generates first qty list that sum up to the given total.
prop_genBalQtysTotalX :: Q.Property
prop_genBalQtysTotalX = Q.forAll genBalQtys $ \(tot, g1, _) ->
  let sx = foldl1 add g1
  in if sx `equivalent` tot
     then QCP.succeeded
     else let r = "planned sum: " ++ showQty tot ++ " actual sum: "
                  ++ showQty sx
          in QCP.failed { QCP.reason = r }

-- | genBalQtys generates a balanced group of quantities.
prop_genBalQtys :: Q.Property
prop_genBalQtys = Q.forAll genBalQtys $ \(tot, g1, g2) ->
  case (g1, g2) of
    (x:xs, y:ys) ->
      let sx = foldl1' add (x:xs)
          sy = foldl1' add (y:ys)
      in if sx `equivalent` sy
         then QCP.succeeded
         else let r = "Different sums. X sum: " ++ showQty sx
                      ++ " Y sum: " ++ showQty sy ++
                      " planned total: " ++ showQty tot
              in QCP.failed { QCP.reason = r }
    _ -> QCP.failed { QCP.reason = "empty quantities list" }

-- | genBalQtys generates valid Qty
prop_genBalQtysValid :: Q.Property
prop_genBalQtysValid = Q.forAll genBalQtys $ \(tot, g1, g2) ->
  validQty tot && all validQty g1 && all validQty g2

-- | > x + y == y + x

prop_commutative :: Qty -> Qty -> Bool
prop_commutative q1 q2 = q1 `add` q2 == q2 `add` q1

-- | Adding q2 to q1 and then taking the difference of q2 gives a
-- LeftBiggerBy q1

prop_addSubtract :: Qty -> Qty -> Bool
prop_addSubtract q1 q2 =
  let diff = (q1 `add` q2) `difference` q2
  in case diff of
      LeftBiggerBy d -> d `equivalent` q1
      _ -> False

-- | add generates valid Qtys
prop_addValid :: Qty -> Qty -> Bool
prop_addValid q1 q2 = validQty $ q1 `add` q2

-- | mult generates valid Qtys
prop_multValid :: Qty -> Qty -> Bool
prop_multValid q1 q2 = validQty $ q1 `mult` q2

-- | genOne generates valid Qtys
prop_genOneValid :: One -> Bool
prop_genOneValid = validQty . unOne

-- | (x `mult` 1) `equivalent` x
prop_multIdentity :: Qty -> One -> Bool
prop_multIdentity x (One q1) = (x `mult` q1) `equivalent` x

-- | newQty fails if mantissa is less than one
prop_newQtyBadMantissa :: Mantissa -> Places -> Q.Property
prop_newQtyBadMantissa m p =
  m < 1 ==> isNothing (newQty m p)

-- | newQty fails if places is less than zero
prop_newQtyBadPlaces :: Mantissa -> Places -> Q.Property
prop_newQtyBadPlaces m p =
  m < 0 ==> isNothing (newQty m p)

-- | difference returns valid Qty
prop_differenceValid :: Qty -> Qty -> Bool
prop_differenceValid q1 q2 = case difference q1 q2 of
  LeftBiggerBy r -> validQty r
  RightBiggerBy r -> validQty r
  Equal -> True

-- | allocate returns valid Qty
prop_allocateValid :: Qty -> (Qty, [Qty]) -> Bool
prop_allocateValid q1 q2 =
  let (r1, r2) = allocate q1 q2
  in validQty r1 && all validQty r2

-- | genEquivalent generates an equivalent Qty
prop_genEquivalent :: Gen Bool
prop_genEquivalent = do
  q1 <- arbitrary
  q2 <- genEquivalent q1
  return $ q1 `equivalent` q2

-- | 'equivalent' fails on different Qty
prop_genNotEquivalent :: Gen Bool
prop_genNotEquivalent = do
  q1 <- arbitrary
  q2 <- genMutate q1
  return . not $ q1 `equivalent` q2

-- | genEquivalent generates valid Qty
prop_genEquivalentValid :: Qty -> Gen QCP.Result
prop_genEquivalentValid q1 = do
  q2 <- genEquivalent q1
  if not . validQty $ q2
    then return QCP.failed { QCP.reason = "invalid qty: " ++ showQty q2 }
    else return QCP.succeeded

-- | genMutate generates valid Qty
prop_genMutateValid :: Qty -> Gen Bool
prop_genMutateValid q1 = do
  q2 <- genMutate q1
  return . validQty $ q2

#endif

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

#ifdef test

-- | Sum of allocation adds up to original Qty

prop_sumAllocate :: Qty -> (Qty, [Qty]) -> Bool
prop_sumAllocate tot ls =
  let (r1, rs) = allocate tot ls
  in foldl1' add (r1:rs) `equivalent` tot

-- | Number of allocations is same as number requested

prop_numAllocate :: Qty -> (Qty, [Qty]) -> Bool
prop_numAllocate tot ls =
  let (_, rs) = allocate tot ls
  in length rs == length (snd ls)

#endif

-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same exponent. Also returns new exponent.
sameExponent
  :: [Qty]
  -> ([Qty], Integer)
sameExponent ls =
  let newExp = maximum . fmap places $ ls
  in (map (increaseExponentTo newExp) ls, newExp)


#ifdef test


-- | sameExponent gives exponents that are all the same
prop_sameExponent :: [Qty] -> Bool
prop_sameExponent qs =
  let (rss, exp') = sameExponent qs
  in all (\q -> places q == exp') rss

#endif

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

#ifdef test

-- | Sum of largest remainder method is equal to total number of seats
prop_sumLargestRemainder
  :: Q.Positive Integer
  -> Q.NonEmptyList (Q.NonNegative Integer)
  -> QCP.Property

prop_sumLargestRemainder tot ls =
  let t = Q.getPositive tot
      l = map Q.getNonNegative . Q.getNonEmpty $ ls
      r = largestRemainderMethod t l
  in sum l > 0 ==> sum r == t
#endif

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
