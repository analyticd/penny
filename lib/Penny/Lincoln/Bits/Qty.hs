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
import Data.Maybe (isJust)
import qualified Test.QuickCheck as Q
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

  , testProperty "prop_addSubtract"
    $ Q.mapSize (min 10) prop_addSubtract

  , testProperty "Sum of allocation adds up to original Qty"
    $ Q.mapSize (min 15) prop_sumAllocate

  , testProperty "Number of allocations is same as number requested"
    $ Q.mapSize (min 15) prop_numAllocate
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

-- | Mutates a Qty so that it is equivalent, but possibly with a
-- different mantissa and exponent.
genEquivalent :: Qty -> Gen Qty
genEquivalent (Qty m p) = do
  expo <- Q.suchThat Q.arbitrarySizedBoundedIntegral (>= (0 :: Int))
  let m' = m * (fromIntegral ((10 :: Int) ^ expo))
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

-- | Chooses one of 'genSized' or 'genRangeInt'.
instance Arbitrary Qty where
  arbitrary = Q.oneof [ genSized
                      , genRangeInt ]

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

#endif

mult :: Qty -> Qty -> Qty
mult (Qty xm xe) (Qty ym ye) = Qty (xm * ym) (xe + ye)


-- | Allocate a Qty proportionally so that the sum of the results adds
-- up to a given Qty. Fails if the allocation cannot be made (e.g. if
-- it is impossible to allocate without overflowing Decimal.) The
-- result will always add up to the given sum.
allocate
  :: Qty
  -- ^ The result will add up to this Qty.

  -> (Qty, [Qty])
  -- ^ Allocate using these Qty (there must be at least one).

  -> (Qty, [Qty])
  -- ^ The length of this list will be equal to the length of the list
  -- of allocations. Each item will correspond to the original
  -- allocation.

allocate tot ls =
  let (tot', ls', e') = sameExponent tot ls
      (tI, lsI) = (mantissa tot', mapPair mantissa ls')
      (seats, (p1, ps), moreE) = growTarget tI lsI
      adjSeats = seats - (genericLength ps + 1)
      del = largestRemainderMethod adjSeats (p1 : ps)
      totE = e' + moreE
      r1:rs = fmap (\m -> Qty (m + 1) totE) del
  in (r1, rs)

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

mapPair :: (a -> b) -> (a, [a]) -> (b, [b])
mapPair f (s, ls) = (f s, map f ls)

-- | Given a list of Decimals, and a single Decimal, return Decimals
-- that are equivalent to the original Decimals, but where all
-- Decimals have the same exponent. Also returns new exponent.
sameExponent
  :: Qty
  -> (Qty, [Qty])
  -> (Qty, (Qty, [Qty]), Integer)
sameExponent dec ls =
  let newExp = max (maximum . fmap places $ (fst ls : snd ls))
                   (places dec)
      dec' = incExp dec
      incExp = increaseExponentTo newExp
      ls' = mapPair incExp ls
  in (dec', ls', newExp)


-- | Given an Integer and a list of Integers, multiply all integers by
-- ten raised to an exponent, so that the first Integer is larger than
-- the count of the number of Integers in the list. Returns
-- the new Integer, new list of Integers, and the exponent used.
--
-- Previously this only grew the first Integer so that it was at least
-- as large as the count of Integers in the list, but this causes
-- problems, as there must be at least one seat for the allocation process.
growTarget
  :: Integer
  -> (Integer, [Integer])
  -> (Integer, (Integer, [Integer]), Integer)
growTarget target is = go target is 0
  where
    len = genericLength (snd is) + 1
    go t xs c =
      let t' = t * 10 ^ c
          xs' = let f x = x * 10 ^ c
                in mapPair f xs
      in if t' > len
         then (t', xs', c)
         else go t' xs' (c + 1)

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
  let fI = fromIntegral
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
