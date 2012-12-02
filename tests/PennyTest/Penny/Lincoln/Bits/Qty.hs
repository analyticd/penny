-- | Tests of Penny quantities

module PennyTest.Penny.Lincoln.Bits.Qty where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Trans.Class (lift)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Applicative ((<$>), (<*>), (<$))
import qualified Test.Framework as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck (arbitrary)
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Property as P
import Data.List (genericLength)
import qualified Penny.Lincoln.Bits.Qty as Q
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Monad (replicateM)
import qualified Data.Foldable as F

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Bits.Qty"
  [ testProperty
    "qty generated from NumberStr pass sanity checks"
    (Ex.resolveT return ex_numberStr)

  , testProperty
    "newQty succeeds and fails as it should"
    prop_newQty

  , testProperty
    "Equivalent Qtys are equivalent, and difference returns Equal"
    (Ex.resolveT return ex_equivalent)

  , testProperty
    "x + 1 - x is equivalent to 1"
    (Ex.resolveT return ex_plus1minus1)

  , testProperty
    "x + y - y is equivalent to x"
    (Ex.resolveT return ex_xPlusY)

  , testProperty
    "x * 1 is equivalent to x"
    (Ex.resolveT return ex_multIdentity)

  , testProperty
    "allocate behaves as expected"
    (Ex.resolveT return ex_allocations)

  , testProperty
    "largestRemainderMethod allocates properly" prop_numParties

  ]

-- | Generates integers.
integers :: Gen (Q.Mantissa, Q.Places)
integers = (\m -> (m, 0))
           <$> G.choose (1, fromIntegral (maxBound :: Int))

-- | How many digits does this number have?
numOfDigits :: Integer -> Integer
numOfDigits = genericLength . show

-- | Generates very small numbers.
verySmall :: Gen (Q.Mantissa, Q.Places)
verySmall = do
  m <- G.choose (1, fromIntegral (maxBound :: Int))
  let nd = numOfDigits m
  p <- G.choose (nd * 10, nd * 100)
  return (m, p)

-- | Generates numbers where the exponent is equal to the number of
-- digits (e.g. .345).
expNumOfDigits :: Gen (Q.Mantissa, Q.Places)
expNumOfDigits = G.sized $ \s -> do
  m <- G.choose (1, max 1 (fromIntegral s))
  return (m, numOfDigits m)

-- | Generates numbers that depend on size parameter. The exponent has
-- up to five more places than in the number generated.
sizedQty :: Gen (Q.Mantissa, Q.Places)
sizedQty = G.sized $ \s -> do
  m <- G.choose (1, max 1 (fromIntegral (s ^ (3 :: Int))))
  p <- G.choose (0, numOfDigits m + 5)
  return (m, p)

-- | Generates large numbers.
large :: Gen (Q.Mantissa, Q.Places)
large = do
  m <- G.choose (10 ^ (7 :: Int), (fromIntegral (maxBound :: Int)))
  p <- G.choose (0, 4)
  return (m, p)


anyGen :: Gen (Q.Mantissa, Q.Places)
anyGen = G.oneof [integers, verySmall, expNumOfDigits, sizedQty, large]

-- | Generates typical numbers.
typical :: Gen (Q.Mantissa, Q.Places)
typical = (,)
          <$> G.choose (1, 10 ^ (7 :: Int))
          <*> G.choose (0, 4)

mkQty
  :: Q.Mantissa
  -> Q.Places
  -> Ex.ExceptionalT P.Result Gen Q.Qty
mkQty m p =
  case Q.newQty m p of
    Nothing -> Ex.throwT P.failed { P.reason = e }
      where e = "failed to make Qty."
    Just q -> q <$ checkQty q

mkQtyG :: Gen (Q.Mantissa, Q.Places)
       -> Ex.ExceptionalT P.Result Gen Q.Qty
mkQtyG g = do
  (m, p) <- lift g
  mkQty m p



-- | Checks to make sure that the Qty has a positive mantissa and a
-- non-negative number of places.
checkQtyVerbose :: Monad m => String -> Q.Qty -> Ex.ExceptionalT P.Result m ()
checkQtyVerbose s q =
  let (m, e) = (Q.mantissa q, Q.places q)
      badM = "bad mantissa: " ++ show m
      badE = "bad exponent: " ++ show e
      d = s ++ ": "
  in case (m > 0, e >= 0) of
    (True, True) -> return ()
    (False, True) -> Ex.throwT P.failed { P.reason = d ++ badM }
    (True, False) -> Ex.throwT P.failed { P.reason = d ++ badE }
    (False, False) ->
      Ex.throwT P.failed { P.reason = d ++ badM ++ " and " ++ badE }

-- | Checks to make sure that the Qty has a positive mantissa and a
-- non-negative number of places.
checkQty :: Monad m => Q.Qty -> Ex.ExceptionalT P.Result m ()
checkQty = checkQtyVerbose ""

nonZeroDigitList :: Gen String
nonZeroDigitList =
  flip G.suchThat (not . all (== '0'))
  . G.listOf1
  . G.choose
  $ ('0', '9')

nonZeroDigitLists :: Gen (String, String)
nonZeroDigitLists = G.suchThat g p
  where
    g = (,) <$> nonZeroDigitList <*> nonZeroDigitList
    good = not . all (== '0')
    p (x, y) = good x && good y

numberStr :: Gen Q.NumberStr
numberStr = G.oneof
  [ Q.Whole <$> nonZeroDigitList
  , Q.WholeRad <$> nonZeroDigitList
  , (\(l, r) -> Q.WholeRadFrac l r) <$> nonZeroDigitLists
  , Q.RadFrac <$> nonZeroDigitList
  ]

-- NumberStr checks

-- | Qty generated from numberStr look like they should. Whole numbers
-- have an exponent of zero. WholeRad also have exponent of
-- zero. WholeRadFrac and RadFrac have positive exponents.

ex_numberStr :: Ex.ExceptionalT P.Result Gen P.Result
ex_numberStr = do
  n <- lift numberStr
  q <- case Q.toQty n of
    Nothing -> Ex.throwT P.failed { P.reason = r }
      where r = "could not generate qty from NumberStr"
    Just qt -> return qt
  checkQtyVerbose (show n) q
  let zeroE = if (Q.places q) == 0
              then return P.succeeded
              else return P.failed { P.reason = e }
                     where e = "exponent is not zero"
      posE l = if (Q.places q) == l
                then return P.succeeded
                else return P.failed { P.reason = e }
                  where e = "exponent is wrong size"
  case n of
    Q.Whole _ -> zeroE
    Q.WholeRad _ -> zeroE
    Q.WholeRadFrac _ f -> posE (genericLength f)
    Q.RadFrac f -> posE (genericLength f)


-- | newQty succeeds and fails as it should
prop_newQty :: Gen P.Result
prop_newQty = do
  m <- QC.arbitrary
  p <- QC.arbitrary
  let mayQ = Q.newQty m p
  case mayQ of
    Nothing -> case (m > 0, p >= 0) of
      (True, True) -> return P.failed
      _ -> return P.succeeded
    Just _ -> do
      case (m > 0, p >= 0) of
        (True, True) -> return P.succeeded
        _ -> return P.failed

-- | Generate two Qtys that should be equivalent.
equivalentQtys :: Gen ((Q.Mantissa, Q.Places), (Q.Mantissa, Q.Places))
equivalentQtys = do
  m <- G.suchThat arbitrary (> 0)
  p <- G.oneof [return 0, G.suchThat arbitrary (>= 0)]
  mult <- G.suchThat arbitrary (>= 0)
  let m' = m * 10 ^ mult
      p' = p + mult
  return ((m, p), (m', p'))

-- | Equivalent Qtys are equivalent, and difference returns Equal.
ex_equivalent :: Ex.ExceptionalT P.Result Gen P.Result
ex_equivalent = do
  ((m1, p1), (m2, p2)) <- lift equivalentQtys
  q1 <- mkQty m1 p1
  q2 <- mkQty m2 p2
  return . P.liftBool $ ( Q.equivalent q1 q2
                          && (Q.difference q1 q2 == Q.Equal))

-- | Generates a quantity of one
genOne :: Ex.ExceptionalT P.Result Gen Q.Qty
genOne = do
  e <- lift $ G.suchThat arbitrary (>= 0)
  let m = 1 * 10 ^ e
  mkQty m e

anyMantissaPlaces :: Gen (Q.Mantissa, Q.Places)
anyMantissaPlaces = G.oneof [ integers, verySmall, large,
                              expNumOfDigits, typical ]

-- | x + 1 - x is equivalent to 1
ex_plus1minus1 :: Ex.ExceptionalT P.Result Gen P.Result
ex_plus1minus1 = do
  (m, p) <- lift anyMantissaPlaces
  q <- mkQty m p
  o1 <- genOne
  o2 <- genOne
  let a = q `Q.add` o1
  case Q.difference a q of
    Q.LeftBiggerBy lbb -> return . P.liftBool $ Q.equivalent lbb o2
    _ -> return P.failed

-- | x + y - y is equivalent to x
ex_xPlusY :: Ex.ExceptionalT P.Result Gen P.Result
ex_xPlusY = do
  (m1, p1) <- lift anyMantissaPlaces
  (m2, p2) <- lift anyMantissaPlaces
  q1 <- mkQty m1 p1
  q2 <- mkQty m2 p2
  let a = q1 `Q.add` q2
  case Q.difference q1 a of
    Q.RightBiggerBy rbb -> return . P.liftBool $ Q.equivalent rbb q2
    _ -> return P.failed

-- | x * 1 is equivalent to x
ex_multIdentity :: Ex.ExceptionalT P.Result Gen P.Result
ex_multIdentity = do
  (m, p) <- lift anyMantissaPlaces
  q <- mkQty m p
  o <- genOne
  return . P.liftBool $ (Q.equivalent (Q.mult q o) q)

-- | Generate parameters for allocate that will have typical financial
-- quantities.
gen_allocate_typical
  :: Ex.ExceptionalT P.Result G.Gen (Q.Qty, NE.NonEmpty Q.Qty)
gen_allocate_typical = do
  d1 <- mkQtyG typical
  d2 <- mkQtyG typical
  len <- lift (G.sized $ \s -> G.choose (0, s))
  ds <- replicateM len (mkQtyG typical)
  return (d1, d2 :| ds)

-- | Generates parameters for allocate that will have a mix of very
-- small and very large numbers (this is most likely to trip up the
-- requirement that all results be non-zero.)
gen_allocate_mixed
  :: Ex.ExceptionalT P.Result G.Gen (Q.Qty, NE.NonEmpty Q.Qty)
gen_allocate_mixed = do
  g1 <- lift $ G.elements
        [verySmall, typical, large]
  d1 <- mkQtyG g1
  g2 <- lift $ G.elements [verySmall, large]
  d2 <- mkQtyG g2
  len <- lift (G.sized $ \s -> G.choose (0, s))
  ds <- replicateM len
        (mkQtyG (G.oneof [verySmall, large]))
  return (d1, d2 :| ds)

-- | Uses one of the given generators. The list must be non-empty.
oneof
  :: [Ex.ExceptionalT P.Result G.Gen a]
  -> Ex.ExceptionalT P.Result G.Gen a
oneof ls
  | null ls = error "PennyTest oneof used with empty list"
  | otherwise = do
      i <- lift $ G.choose (0, length ls - 1)
      ls !! i


-- | Does the sum of the results of allocate add up to the original
-- sum requested?
allocateSum :: Q.Qty -> NonEmpty Q.Qty -> Bool
allocateSum q ls = (F.foldl1 Q.add ls) `Q.equivalent` q

-- | Tests three things about allocations: they add up to the correct
-- value, the number of allocations made is the same as the number
-- requested, and none of the results are zero or negative.
ex_allocations
  :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_allocations = do
  (tot, ls) <- oneof [gen_allocate_typical, gen_allocate_mixed]
  let a = Q.allocate tot ls
  let e = "target: " ++ show tot ++ " result: " ++ show a
          ++ " votes: " ++ show ls
  mapM_ (checkQtyVerbose e) . NE.toList $ a
  if allocateSum tot a then return ()
    else let r = "allocateSum failed. " ++ e
         in Ex.throwT (P.failed { P.reason = r })
  if ((length . F.toList $ ls) == (length . F.toList $ a))
    then return P.succeeded
    else return P.failed { P.reason = "lengths do not match" }

-- | Number of parties allocated by largestRemainderMethod is always
-- equal to the number of parties requested, and that the sum of the
-- allocation is equal to what was requested
prop_numParties :: Gen Bool
prop_numParties = G.sized $ \s -> do
  let genInt = G.choose (1, max 1 (fromIntegral s))
  ts <- genInt
  tvs <- G.listOf1 genInt
  let r = Q.largestRemainderMethod ts tvs
  return $ sum r == ts && length r == length tvs
