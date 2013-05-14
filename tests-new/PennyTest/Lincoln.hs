{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module PennyTest.Lincoln where

import Control.Monad (liftM2, liftM5, replicateM, guard)
import Data.List (foldl1')
import Data.Maybe (isJust, isNothing, catMaybes)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Time as T
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as QG
import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.All as A
import Test.QuickCheck (Gen, Arbitrary, arbitrary, (==>))
import qualified Penny.Lincoln as L
import Penny.Lincoln.Equivalent ((==~))
import Data.Text (Text)
import qualified Data.Text as X
import System.Random.Shuffle (shuffle')
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

--
-- # Qty
--

failMsg :: Monad m => String -> m a
failMsg s = fail $ s ++ ": generation failed"

-- | Generates Qty with the exponent restricted to a reasonable
-- size. Currently this means it is between 0 and 5, inclusive. Big
-- mantissas are not a problem, but big exponents quickly make the
-- tests practically un-runnable.
genReasonableExp :: Gen L.Qty
genReasonableExp = Q.sized $ \s -> do
  m <- Q.suchThat Q.arbitrarySizedBoundedIntegral (> (0 :: Int))
  p <- Q.choose (0 :: Int, 5)
  maybe (failMsg "genSmallSized") return
    $ L.newQty (fromIntegral m) (fromIntegral p)

maxExponent :: Integer
maxExponent = 5

genExponent :: Gen Integer
genExponent = Q.choose (0, maxExponent)

-- | Mutates a Qty so that it is equivalent, but possibly with a
-- different mantissa and exponent.
genEquivalent :: L.Qty -> Gen L.Qty
genEquivalent q = do
  let (m, p) = (L.mantissa q, L.places q)
  expo <- genExponent
  let m' = m * (10 ^ expo)
      p' = p + (fromIntegral expo)
  maybe (failMsg "genEquivalent") return $ L.newQty m' p'

-- | Mutates a Qty so that it is not equivalent. Changes either the
-- mantissa or the exponent or both.
genMutate :: L.Qty -> Gen L.Qty
genMutate q = do
  let (m, p) = (L.mantissa q, L.places q)
  (changeMantissa, changeExp) <-
    Q.suchThat (liftM2 (,) arbitrary arbitrary)
    (/= (False, False))
  m' <- if changeMantissa then mutateAtLeast1 m else return m
  p' <- if changeExp then mutateExponent p else return p
  maybe (failMsg "genMutate") return $ L.newQty m' p'

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
mutateExponent :: Integer -> Gen Integer
mutateExponent i = Q.suchThat (Q.choose (0, maxExponent)) (/= i)

-- | Generates one, with different exponents.
genOne :: Gen L.Qty
genOne = do
  p <- Q.choose (0, maxExponent)
  maybe (failMsg "genOne") return $ L.newQty (1 * 10 ^ p) p

-- | Chooses one of 'genSized' or 'genRangeInt' or 'genSmallExp'.
instance Arbitrary L.Qty where
  arbitrary = Q.oneof [ genSmallSized, genSmallExp ]

-- | Mantissas are always greater than zero.
prop_mantissa :: L.Qty -> Bool
prop_mantissa q = L.mantissa q > 0

-- | Exponent is always at least zero
prop_exponent :: L.Qty -> Bool
prop_exponent q = L.places q >= 0

-- | newQty passes if exponent is at least zero and if mantissa is
-- greater than zero.

prop_newQtySucceeds :: L.Mantissa -> L.Places -> Q.Property
prop_newQtySucceeds m p =
  m > 0 ==> p >= 0 ==> isJust (L.newQty m p)

-- | True if this is a valid Qty; that is, the mantissa is greater
-- than 0 and the number of places is greater than or equal to 0.
validQty :: L.Qty -> Bool
validQty q = L.mantissa q > 0 && L.places q >= 0


maxSizeList :: Arbitrary a => Int -> Gen [a]
maxSizeList i = Q.sized $ \s -> do
  len <- Q.choose (0, min s i)
  Q.vector len

-- | Generates a group of balanced quantities.
genBalQtys :: Gen (L.Qty, [L.Qty], [L.Qty])
genBalQtys = maxSize 5 $ do
  total <- arbitrary
  group1alloc1 <- arbitrary
  group1allocRest <- arbitrary
  group2alloc1 <- arbitrary
  group2allocRest <- arbitrary
  let (g1r1, g1rs) = L.allocate total (group1alloc1, group1allocRest)
      (g2r1, g2rs) = L.allocate total (group2alloc1, group2allocRest)
  return $ (total, g1r1 : g1rs, g2r1 : g2rs)

-- | genBalQtys generates first qty list that sum up to the given total.
prop_genBalQtysTotalX :: Q.Property
prop_genBalQtysTotalX = Q.forAll genBalQtys $ \(tot, g1, _) ->
  let sx = foldl1 L.add g1
  in if sx ==~ tot
     then QCP.succeeded
     else let r = "planned sum: " ++ show tot ++ " actual sum: "
                  ++ show sx
          in QCP.failed { QCP.reason = r }

-- | genBalQtys generates a balanced group of quantities.
prop_genBalQtys :: Q.Property
prop_genBalQtys = Q.forAll genBalQtys $ \(tot, g1, g2) ->
  case (g1, g2) of
    (x:xs, y:ys) ->
      let sx = foldl1' L.add (x:xs)
          sy = foldl1' L.add (y:ys)
      in if sx ==~ sy
         then QCP.succeeded
         else let r = "Different sums. X sum: " ++ show sx
                      ++ " Y sum: " ++ show sy ++
                      " planned total: " ++ show tot
              in QCP.failed { QCP.reason = r }
    _ -> QCP.failed { QCP.reason = "empty quantities list" }

-- | > x + y == y + x

prop_commutative :: L.Qty -> L.Qty -> Bool
prop_commutative q1 q2 = q1 `L.add` q2 == q2 `L.add` q1

-- | Adding q2 to q1 and then taking the difference of q2 gives a
-- LeftBiggerBy q1

prop_addSubtract :: L.Qty -> L.Qty -> Bool
prop_addSubtract q1 q2 =
  let diff = (q1 `L.add` q2) `L.difference` q2
  in case diff of
      L.LeftBiggerBy d -> d ==~ q1
      _ -> False

-- | add generates valid Qtys
prop_addValid :: L.Qty -> L.Qty -> Bool
prop_addValid q1 q2 = validQty $ q1 `L.add` q2

-- | mult generates valid Qtys
prop_multValid :: L.Qty -> L.Qty -> Bool
prop_multValid q1 q2 = validQty $ q1 `L.mult` q2

newtype One = One { unOne :: L.Qty }
  deriving (Eq, Show)

instance Arbitrary One where arbitrary = fmap One genOne

-- | genOne generates valid Qtys
prop_genOneValid :: One -> Bool
prop_genOneValid = validQty . unOne

-- | (x `mult` 1) `equivalent` x
prop_multIdentity :: L.Qty -> One -> Bool
prop_multIdentity x (One q1) = (x `L.mult` q1) ==~ x

-- | newQty fails if mantissa is less than one
prop_newQtyBadMantissa :: L.Mantissa -> L.Places -> Q.Property
prop_newQtyBadMantissa m p =
  m < 1 ==> isNothing (L.newQty m p)

-- | newQty fails if places is less than zero
prop_newQtyBadPlaces :: L.Mantissa -> L.Places -> Q.Property
prop_newQtyBadPlaces m p =
  m < 0 ==> isNothing (L.newQty m p)

-- | difference returns valid L.Qty
prop_differenceValid :: L.Qty -> L.Qty -> Bool
prop_differenceValid q1 q2 = case L.difference q1 q2 of
  L.LeftBiggerBy r -> validQty r
  L.RightBiggerBy r -> validQty r
  L.Equal -> True

-- | allocate returns valid Qty
prop_allocateValid :: L.Qty -> (L.Qty, [L.Qty]) -> Bool
prop_allocateValid q1 q2 =
  let (r1, r2) = L.allocate q1 q2
  in validQty r1 && all validQty r2

-- | genEquivalent generates an equivalent Qty
prop_genEquivalent :: L.Qty -> Gen Bool
prop_genEquivalent q1 = do
  q2 <- genEquivalent q1
  return $ q1 ==~ q2

-- | 'equivalent' fails on different Qty
prop_genNotEquivalent :: L.Qty -> Gen Bool
prop_genNotEquivalent q1 = do
  q2 <- genMutate q1
  return . not $ q1 ==~ q2

-- | newQty succeeds and fails as it should, and generates valid Qty
prop_newQty :: L.Mantissa -> L.Places -> Bool
prop_newQty m p = case (m > 0, p >= 0) of
  (True, True) -> case L.newQty m p of
    Nothing -> False
    Just q -> L.mantissa q == m && L.places q == p
  _ -> isNothing (L.newQty m p)

-- | Sum of allocation adds up to original Qty

prop_sumAllocate :: L.Qty -> (L.Qty, [L.Qty]) -> Bool
prop_sumAllocate tot ls =
  let (r1, rs) = L.allocate tot ls
  in foldl1' L.add (r1:rs) ==~ tot

-- | Number of allocations is same as number requested

prop_numAllocate :: L.Qty -> (L.Qty, [L.Qty]) -> Bool
prop_numAllocate tot ls =
  let (_, rs) = L.allocate tot ls
  in length rs == length (snd ls)

-- | Sum of largest remainder method is equal to total number of seats
prop_sumLargestRemainder
  :: Q.Positive Integer
  -> Q.NonEmptyList (Q.NonNegative Integer)
  -> QCP.Property

prop_sumLargestRemainder tot ls =
  let t = Q.getPositive tot
      l = map Q.getNonNegative . Q.getNonEmpty $ ls
      r = L.largestRemainderMethod t l
  in sum l > 0 ==> sum r == t

--
-- # DateTime
--

instance Arbitrary L.TimeZoneOffset where
  arbitrary = Q.choose (-840, 840)
    >>= maybe (failMsg "timeZoneOffset") return . L.minsToOffset

instance Arbitrary L.Hours where
  arbitrary = Q.choose (0, 23)
    >>= maybe (failMsg "hours") return . L.intToHours

instance Arbitrary L.Minutes where
  arbitrary = Q.choose (0, 59)
    >>= maybe (failMsg "minutes") return . L.intToMinutes

instance Arbitrary L.Seconds where
  arbitrary = Q.choose (0, 60)
    >>= maybe (failMsg "seconds") return . L.intToSeconds

genDay :: Q.Gen T.Day
genDay = fmap T.ModifiedJulianDay $ Q.choose (b, e)
  where
    b = T.toModifiedJulianDay $ T.fromGregorian 1000 01 01
    e = T.toModifiedJulianDay $ T.fromGregorian 3000 01 01

instance Arbitrary L.DateTime where
  arbitrary = liftM5 L.DateTime genDay
    arbitrary arbitrary arbitrary arbitrary

--
-- # Open
--

maxSize :: Int -> Gen a -> Gen a
maxSize i g = Q.sized $ \s -> Q.resize (min i s) g

-- | Generates a Text from valid Unicode chars.
genText :: Gen Text
genText = maxSize 5
  $ fmap X.pack $ Q.oneof [ Q.listOf ascii, Q.listOf rest ]
  where
    ascii = Q.choose (toEnum 32, toEnum 126)
    rest = Q.suchThat (Q.choose (minBound, maxBound))
                       (\c -> c < '\xd800' || c > '\xdfff')

instance Arbitrary L.SubAccount where
  arbitrary = fmap L.SubAccount genText

instance Arbitrary L.Account where
  arbitrary = fmap L.Account arbitrary

instance Arbitrary L.Amount where
  arbitrary = liftM2 L.Amount arbitrary arbitrary

instance Arbitrary L.Commodity where
  arbitrary = fmap L.Commodity genText

instance Arbitrary L.DrCr where
  arbitrary = Q.elements [L.Debit, L.Credit]

instance Arbitrary L.Entry where
  arbitrary = liftM2 L.Entry arbitrary arbitrary

instance Arbitrary L.Flag where
  arbitrary = fmap L.Flag genText

instance Arbitrary L.Memo where
  arbitrary = fmap L.Memo $ Q.listOf genText

instance Arbitrary L.Number where
  arbitrary = fmap L.Number genText

instance Arbitrary L.Payee where
  arbitrary = fmap L.Payee genText

instance Arbitrary L.Tag where
  arbitrary = fmap L.Tag genText

instance Arbitrary L.Tags where
  arbitrary = fmap L.Tags $ Q.listOf arbitrary

instance Arbitrary L.TopLineLine where
  arbitrary = fmap L.TopLineLine Q.arbitrarySizedBoundedIntegral

instance Arbitrary L.TopMemoLine where
  arbitrary = fmap L.TopMemoLine Q.arbitrarySizedBoundedIntegral

instance Arbitrary L.Side where
  arbitrary = Q.elements [L.CommodityOnLeft, L.CommodityOnRight]

instance Arbitrary L.SpaceBetween where
  arbitrary = Q.elements [L.SpaceBetween, L.NoSpaceBetween]

instance Arbitrary L.Filename where
  arbitrary = fmap L.Filename genText

instance Arbitrary L.PriceLine where
  arbitrary = fmap L.PriceLine Q.arbitrarySizedBoundedIntegral

instance Arbitrary L.PostingLine where
  arbitrary = fmap L.PostingLine Q.arbitrarySizedBoundedIntegral

instance Arbitrary L.GlobalPosting where
  arbitrary = fmap L.GlobalPosting arbitrary

instance Arbitrary L.FilePosting where
  arbitrary = fmap L.FilePosting arbitrary

instance Arbitrary L.GlobalTransaction where
  arbitrary = fmap L.GlobalTransaction arbitrary

instance Arbitrary L.FileTransaction where
  arbitrary = fmap L.FileTransaction arbitrary

instance Arbitrary L.Serial where
  arbitrary = do
    ls <- Q.listOf1 (return ())
    let sers = L.serialItems const ls
    fmap head $ shuffle sers

-- | Shuffles a list.
shuffle :: [a] -> Gen [a]
shuffle ls = QG.MkGen $ \g _ ->
  shuffle' ls (length ls) g

--
-- # Ents
--

-- | Generates restricted ents
genRestricted :: Arbitrary a => Gen (L.Ents a)
genRestricted = liftM5 L.rEnts arbitrary arbitrary arbitrary
                arbitrary arbitrary

-- | Generates a group of balanced entries.
genBalEntries :: Gen ([L.Entry])
genBalEntries = do
  (_, qDeb, qCred) <- genBalQtys
  let qtysAndDrCrs = map (\en -> (L.Debit, en)) qDeb
                     ++ map (\en -> (L.Credit, en)) qCred
  cty <- arbitrary
  let mkEn (drCr, qty) = L.Entry drCr (L.Amount qty cty)
  return $ map mkEn qtysAndDrCrs

newtype BalEntries = BalEntries
  { unBalEntries :: [L.Entry] }
  deriving (Eq, Show)

instance Arbitrary BalEntries where
  arbitrary = fmap BalEntries genBalEntries

-- | Generates a list of entries. At most, one of these is Inferred.
genEntriesWithInfer :: Gen [(L.Entry, L.Inferred)]
genEntriesWithInfer = do
  nGroups <- Q.suchThat Q.arbitrarySizedIntegral (> 0)
  entries <- fmap concat $ replicateM nGroups genBalEntries
  makeNothing <- arbitrary
  let entries' = if makeNothing
        then (head entries, L.Inferred)
             : map (\en -> (en, L.NotInferred)) (tail entries)
        else map (\en -> (en, L.NotInferred)) entries
  shuffle entries'


-- | Gets a single inferred entry from a balance, if possible.
inferredVal :: [Maybe L.Entry] -> Maybe L.Entry
inferredVal ls = do
  guard ((length . filter id . map isNothing $ ls) == 1)
  let bal = mconcat
            . map L.entryToBalance
            . catMaybes
            $ ls
  case L.isBalanced bal of
    L.Inferable e -> Just e
    _ -> Nothing

-- | genEntriesWithInfer is inferable
prop_genEntries :: Q.Property
prop_genEntries = Q.forAll genEntriesWithInfer $
  \ps -> L.Inferred `elem` (map snd ps)
         ==> isJust (inferredVal (map toEn ps))
  where
    toEn (en, inf) = if inf == L.Inferred then Nothing else Just en

-- | genBalEntries generates groups that are balanced.
prop_balEntries :: BalEntries -> Bool
prop_balEntries
  = M.null
  . L.unBalance
  . L.removeZeroCommodities
  . mconcat
  . map L.entryToBalance
  . unBalEntries

-- | 'views' gives as many views as there were postings

prop_numViews :: L.Ents m -> Bool
prop_numViews t = (length . L.views $ t) == (length . L.unEnts $ t)

newtype NonRestricted a = NonRestricted
  { unNonRestricted :: [(Maybe L.Entry, a)] }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonRestricted a) where
  arbitrary = do
    ls <- genEntriesWithInfer
    metas <- Q.vector (length ls)
    let mkPair (en, inf) mt = case inf of
          L.Inferred -> (Nothing, mt)
          L.NotInferred -> (Just en, mt)
    return . NonRestricted $ zipWith mkPair ls metas

genNonRestricted :: Arbitrary a => Gen (L.Ents a)
genNonRestricted =
  arbitrary
  >>= maybe (failMsg "genNonRestricted") return
      . L.ents
      . unNonRestricted

instance Arbitrary a => Arbitrary (L.Ents a) where
  arbitrary = Q.oneof [ genNonRestricted
                       , genRestricted ]

-- | Ents always have at least two postings
prop_twoPostings :: L.Ents a -> Bool
prop_twoPostings e = length (L.unEnts e) > 1

-- | Ents are always balanced
prop_balanced :: L.Ents a -> Bool
prop_balanced t = L.isBalanced bal == L.Balanced
  where
    bal = mconcat
          . map L.entryToBalance
          . map L.entry
          . L.unEnts
          $ t

-- | Ents contain no more than one inferred posting
prop_inferred :: L.Ents a -> Bool
prop_inferred t =
  (length . filter (== L.Inferred) . map L.inferred . L.unEnts $ t)
  < 2

newtype BalQtys = BalQtys { _unBalQtys :: ([L.Qty], [L.Qty]) }
  deriving (Eq, Show)

-- | 'ents' makes ents as it should. Also tests whether
-- the 'Arbitrary' instance of 'NonRestricted' is behaving as it
-- should.

prop_ents :: NonRestricted a -> Bool
prop_ents (NonRestricted ls) = isJust $ L.ents ls

-- | NonRestricted makes ents with two postings
prop_entsTwoPostings :: NonRestricted a -> Bool
prop_entsTwoPostings (NonRestricted ls) = case L.ents ls of
  Nothing -> False
  Just t -> prop_twoPostings t

-- | 'rEnts' behaves as it should

prop_rEnts
  :: L.Commodity
  -> L.DrCr
  -> (L.Qty, a)
  -> [(L.Qty, a)]
  -> a
  -> Bool
prop_rEnts c dc pr ls mt =
  let t = L.rEnts c dc pr ls mt
  in prop_twoPostings t && prop_balanced t && prop_inferred t

qtyTests :: Test
qtyTests = testGroup "Qty" []

runTests = $(A.forAllProperties) (Q.quickCheckWithResult Q.stdArgs)

