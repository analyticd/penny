module PennyTest.Lincoln.Bits where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Decimal as D
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromJust)
import qualified Data.Time as DT
import Data.Word (Word8)
import qualified Penny.Lincoln.Bits as B
import PennyTest.Lincoln.TextNonEmpty ()
import qualified System.Random as R
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, Arbitrary, Gen, choose, suchThat,
                        sized, resize, listOf)

instance Q.Arbitrary B.SubAccountName where
  arbitrary = B.SubAccountName <$> arbitrary

instance Q.Arbitrary B.Account where
  arbitrary = B.Account <$> arbitrary

instance Q.Arbitrary B.Amount where
  arbitrary = B.Amount <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.SubCommodity where
  arbitrary = B.SubCommodity <$> arbitrary

instance Q.Arbitrary B.Commodity where
  arbitrary = B.Commodity <$> arbitrary

instance Q.Arbitrary DT.DiffTime where
  arbitrary = DT.secondsToDiffTime
              <$> (Q.suchThat arbitrary (\s -> s >= 0 && s < 86400))

instance Q.Arbitrary DT.UTCTime where
  arbitrary = DT.UTCTime <$> arbitrary <*> arbitrary

instance Q.Arbitrary DT.Day where
  arbitrary = DT.ModifiedJulianDay <$> choose (20000, 100000)

instance Q.Arbitrary DT.TimeOfDay where
  arbitrary = DT.TimeOfDay
              <$> choose (0, 23)
              <*> choose (0, 59)
              <*> (fromIntegral <$> choose (0 :: Int, 59))

instance Q.Arbitrary DT.LocalTime where
  arbitrary = DT.LocalTime <$> arbitrary <*> arbitrary

instance Q.Arbitrary DT.TimeZone where
  arbitrary = DT.TimeZone
              <$> choose ((-1339), 1339)
              <*> arbitrary
              <*> resize 4 (listOf arbitrary)

instance Q.Arbitrary DT.ZonedTime where
  arbitrary = DT.ZonedTime <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.DateTime where
  arbitrary = B.DateTime <$> arbitrary

instance Arbitrary B.DrCr where
  arbitrary = Q.oneof [pure B.Debit, pure B.Credit]

instance Arbitrary B.Entry where
  arbitrary = B.Entry <$> arbitrary <*> arbitrary

instance Arbitrary B.Flag where
  arbitrary = B.Flag <$> arbitrary

instance Arbitrary B.MemoLine where
  arbitrary = B.MemoLine <$> arbitrary

instance Arbitrary B.Memo where
  arbitrary = B.Memo <$> arbitrary

instance Arbitrary B.Number where
  arbitrary = B.Number <$> arbitrary

instance Arbitrary B.Payee where
  arbitrary = B.Payee <$> arbitrary

instance Arbitrary B.From where
  arbitrary = B.From <$> arbitrary

instance Arbitrary B.To where
  arbitrary = B.To <$> arbitrary

instance Arbitrary B.CountPerUnit where
  arbitrary = B.CountPerUnit <$> arbitrary

instance Arbitrary B.Price where
  arbitrary = mkPrice <$> Q.suchThat g p where
    g = (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    p (f, t, c) = case B.newPrice f t c of
      Nothing -> False
      Just _ -> True
    mkPrice (f, t, c) = fromJust $ B.newPrice f t c

instance Arbitrary B.PricePoint where
  arbitrary = B.PricePoint <$> arbitrary <*> arbitrary

-- | Creates random Decimals with a distribution a little more
-- interesting than the Arbitrary that comes with Data.Decimal.
-- Also, only returns Decimals >= 0.
randomDecimal :: Gen D.Decimal
randomDecimal = sized $ \s -> do
  places <- choose (0, (min s (fromIntegral maxDecimalPlaces)))
  mantissa <- choose (1, (max 1 $ fromIntegral s ^ mantissaExponent))
  return $ D.Decimal (fromIntegral places) mantissa

maxDecimalPlaces :: Word8
maxDecimalPlaces = 10

mantissaExponent :: Int
mantissaExponent = 10

instance Q.Arbitrary B.Qty where
  arbitrary = B.partialNewQty <$> randomDecimal

instance Arbitrary B.Tag where
  arbitrary = B.Tag <$> arbitrary

instance Arbitrary B.Tags where
  arbitrary = B.Tags <$> arbitrary

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

type NEDrCrQty = (B.DrCr, NE.NonEmpty B.Qty)


-- | Returns a triple (a, b, c) where:
--
-- * a is a Debit or Credit and a list of random quantities
--
-- * b is a Debit or Credit (the opposite of whatever was given in a)
-- and list of random quantities that add up to the sum of the
-- quantities given in a
--
-- * c is, if possible, a Debit or Credit (the opposite of whatever
-- was given in a) and a list of random quantities that add up to
-- something less than the sum of the quantities given in a
randEntries :: Gen (NEDrCrQty, NEDrCrQty, Maybe NEDrCrQty)
randEntries = let
  mkq = B.partialNewQty
  f dc (os, ss, mss) = let
    dcOther = case dc of
      B.Debit -> B.Credit
      B.Credit -> B.Debit
    os' = (dc, fmap mkq os)
    ss' = (dcOther, fmap mkq ss)
    mss' = (,) <$> pure dcOther <*> (fmap mkq <$> mss)
    in (os', ss', mss')
  in f <$> arbitrary <*> randDecTriple
    
    
type NEDecimal = NE.NonEmpty D.Decimal

-- | Generate a list of random positive Decimals, a list of Decimals
-- that add up to that Decimal, and (if possible) a list of Decimals
-- that add up to some quantity that is less than the Decimal.
randDecTriple :: Gen (NEDecimal, NEDecimal, Maybe NEDecimal)
randDecTriple = do
  d <- NE.fromList <$> Q.listOf1 randomDecimal
  ds <- addsUpTo (F.sum d)
  mayBZ <- betweenZero (F.sum d)
  bz <- case mayBZ of
    Nothing -> return Nothing
    Just b -> Just <$> addsUpTo b
  return (d, ds, bz)
        

-- | Given a positive decimal p, return a decimal that has the same
-- number of decimal places and is in the open-ended range (0,
-- p). Returns Nothing if p is positive but there are no numbers
-- between it and 0. Applies 'error' of p is not positive.
betweenZero :: D.Decimal -> Gen (Maybe D.Decimal)
betweenZero (D.Decimal p m)
  | m < 1 = error "betweenZero: argument not positive"
  | m == 1 = pure Nothing
  | otherwise = Just . D.Decimal p
                <$> choose (1, m - 1)

-- | Generate a random list of Decimals that adds up to the given
-- Decimal.
addsUpTo :: D.Decimal -> Gen (NE.NonEmpty D.Decimal)
addsUpTo d = (fmap unDec) <$> (addsUpToDec (Dec d))

addsUpToDec :: Dec -> Gen (NE.NonEmpty Dec)
addsUpToDec t = do
  its <- suchThat arbitrary (>= 1)
  ns <- newNum t its []
  return (NE.fromList (filter (/= (Dec (D.Decimal 0 0))) ns))
  

-- | Given a target sum, the sum of the list so far, and the number of
-- iterations left, generate a new random number and add it to the
-- sequence. If the number of iterations left is 1, then add whatever
-- amount is necessary to make the list add up to the target sum.
type Target = Dec
type SumSoFar = Dec
type NumLeft = Int
newNum :: Target -> NumLeft -> [Dec] -> Gen [Dec]
newNum (Dec t) l rs = let
  soFar = if null rs then decZero else Dec (sum (map unDec rs))
  needed = Dec (t - (unDec soFar)) in
  if l == 1
  then return (needed : rs)
  else do
    n <- choose (decZero, needed)
    newNum (Dec t) (pred l) (n:rs)

-- | Given two Decimals, make their exponents the same.
equalizeExponents :: Dec -> Dec -> (Dec, Dec)
equalizeExponents (Dec d1) (Dec d2) = (Dec d1', Dec d2') where
  mkEq l r = l + r - r
  d1' = mkEq d1 d2
  d2' = mkEq d2 d1


instance R.Random Dec where
  --randomR :: RandomGen g => (a, a) -> g -> (a, g)
  randomR (low, high) g = let
    ((Dec l), (Dec h)) = equalizeExponents low high
    (D.Decimal e lI, D.Decimal _ hI) = (l, h)
    (newI, g') = R.randomR (lI, hI) g
    in (Dec (D.Decimal e newI), g')
  random g = (a, g') where
    (newI, g') = R.randomR (0, 999) g
    a = Dec (D.Decimal 3 newI)

newtype Dec = Dec { unDec :: D.DecimalRaw Integer }
              deriving (Show, Eq)

decZero :: Dec
decZero = Dec $ D.Decimal 0 0
