-- | Tests of Penny quantities

module PennyTest.Penny.Lincoln.Bits.Qty where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Trans.Class (lift)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Applicative ((<$>), (<*>))
import qualified Test.Framework as TF
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as G
import qualified Test.QuickCheck.Property as P
import qualified Data.Decimal as D
import Data.Maybe (isNothing)
import qualified Penny.Lincoln.Bits.Qty as Q
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Monad (replicateM)
import qualified Data.Foldable as F

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Bits.Qty"
  [ testProperty "((x `add` y) `subt` y) == y" prop_addSubt
  , testProperty "(x * 1) == x" prop_addOne
  , testProperty "difference works as expected" prop_difference
  , testProperty
    "newQty rejects decimals less than or equal to zero"
    prop_newQty

  , testProperty
    "allocations are correct"
    prop_allocations

  ]

-- | Generates half verySmallDecimal generators, half typicalDecimal
-- generators.
decimalGen :: G.Gen (G.Gen D.Decimal)
decimalGen = G.elements [verySmallDecimal, typicalDecimal]

-- | Generates very small positive Decimals.
verySmallDecimal :: G.Gen D.Decimal
verySmallDecimal =
  D.Decimal
  <$> G.choose (10, 255)
  <*> G.choose (1, 1 * 10 ^ (6 :: Int))

-- | Generates very large positive Decimals.
veryLargeDecimal :: G.Gen D.Decimal
veryLargeDecimal =
  D.Decimal
  <$> G.choose (0, 5)
  <*> G.choose (1 * 10 ^ (5 :: Int), (fromIntegral (maxBound :: Int)))

-- | Generates typical decimals in a financial setting. Have exponents
-- between 0 and 5, and mantissas that are between 1 and 2 raised to
-- the power of the size parameter.
typicalDecimal :: G.Gen D.Decimal
typicalDecimal = G.sized $ \s ->
  D.Decimal
  <$> G.choose (0, 5)
  <*> G.choose (1, 2 ^ s)

-- | Generates 1. The exponent depends on the size parameter (but
-- never exceeds 255.) The mantissa adjusts so the result is 1.
one :: G.Gen D.Decimal
one = G.sized $ \s -> do
  e <- G.choose (0, min (fromIntegral s) 255)
  return $ D.Decimal e (1 * 10 ^ (fromIntegral e :: Int))

genQty
  :: (G.Gen D.Decimal)
  -> Ex.ExceptionalT P.Result G.Gen Q.Qty
genQty g = do
  d <- lift g
  case Q.newQty d of
    Nothing -> Ex.throwT (P.failed { P.reason = r })
      where
        r = "Qty generation failed from decimal " ++ show d
    Just q -> return q

-- | > (x + y - y) == x
ex_addSubt :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_addSubt = do
  g <- lift decimalGen
  x <- genQty g
  y <- genQty g
  case (x `Q.add` y) `Q.subt` y of
    Nothing -> return P.failed
    Just r -> return $ P.liftBool (r == x)

-- | > (x + y - y) == x
prop_addSubt :: QC.Gen P.Result
prop_addSubt = Ex.resolveT return ex_addSubt

-- | > (x * 1) == x
ex_multOne :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_multOne = do
  g <- lift decimalGen
  x <- genQty g
  o <- genQty one
  return $ P.liftBool ((x `Q.mult` o) == x)

-- | > (x * 1) == x
prop_addOne :: G.Gen P.Result
prop_addOne = Ex.resolveT return ex_multOne

-- | Difference works as expected.
ex_difference :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_difference = do
  g <- lift decimalGen
  x <- genQty g
  y <- genQty g
  let diff = Q.difference x y
  if x == y
    then return $ P.liftBool (diff == Q.Equal)
    else case Q.subt x y of
          Just s ->
            let expected = Q.LeftBiggerBy s
            in return $ P.liftBool (expected == diff)
          Nothing ->
            case Q.subt y x of
              Nothing -> return P.failed
              Just s' ->
                let expected = Q.RightBiggerBy s'
                in return $ P.liftBool (expected == diff)

-- | difference works as expected
prop_difference :: G.Gen P.Result
prop_difference = Ex.resolveT return ex_difference

-- | newQty rejects decimals less than zero, and zeroes
prop_newQty :: G.Gen P.Result
prop_newQty = do
  e <- G.choose (0, 255)
  m <- G.oneof [ G.choose ((fromIntegral (minBound :: Int)), -1)
               , return 0 ]
  let d = D.Decimal e m
  return $ P.liftBool (isNothing (Q.newQty d))

-- | Generate parameters for allocate that will have typical financial
-- quantities.
gen_allocate_typical
  :: Ex.ExceptionalT P.Result G.Gen (Q.Qty, NE.NonEmpty Q.Qty)
gen_allocate_typical = do
  d1 <- genQty typicalDecimal
  d2 <- genQty typicalDecimal
  len <- lift (G.sized $ \s -> G.choose (0, s))
  ds <- replicateM len (genQty typicalDecimal)
  return (d1, d2 :| ds)

-- | Generates parameters for allocate that will have a mix of very
-- small and very large numbers (this is most likely to trip up the
-- requirement that all results be non-zero.)
gen_allocate_mixed
  :: Ex.ExceptionalT P.Result G.Gen (Q.Qty, NE.NonEmpty Q.Qty)
gen_allocate_mixed = do
  g1 <- lift $ G.elements
        [verySmallDecimal, typicalDecimal, veryLargeDecimal]
  d1 <- genQty g1
  g2 <- lift $ G.elements [verySmallDecimal, veryLargeDecimal]
  d2 <- genQty g2
  len <- lift (G.sized $ \s -> G.choose (0, s))
  ds <- replicateM len
        (genQty (G.oneof [verySmallDecimal, veryLargeDecimal]))
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
allocateSum q ls = (F.foldl1 Q.add ls) == q

-- | Tests three things about allocations: they add up to the correct
-- value, the number of allocations made is the same as the number
-- requested, and none of the results are zero or negative.
ex_allocations
  :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_allocations = do
  (tot, ls) <- oneof [gen_allocate_typical, gen_allocate_mixed]
  case Q.allocate tot ls of
    Nothing -> return P.rejected
    Just r ->
      return $ P.liftBool
        ((allocateSum tot r)
          && ((length . F.toList $ ls) == (length . F.toList $ r))
          && (F.all (\q -> Q.unQty q > D.Decimal 0 0) ls))


-- | Sum of allocations adds up to requested value.
prop_allocations :: G.Gen P.Result
prop_allocations = Ex.resolveT return ex_allocations

