module PennyTest.Lincoln.Bits where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Decimal as D
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromJust)
import qualified Data.Time as DT
import qualified Penny.Lincoln.Bits as B
import PennyTest.Lincoln.TextNonEmpty ()
import qualified System.Random as R
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, Arbitrary, Gen, choose, suchThat,
                        resize)

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


instance Q.Arbitrary DT.Day where
  arbitrary = DT.ModifiedJulianDay <$> (Q.suchThat arbitrary (>= 0))

instance Q.Arbitrary DT.DiffTime where
  arbitrary = DT.secondsToDiffTime
              <$> (Q.suchThat arbitrary (\s -> s >= 0 && s < 86400))

instance Q.Arbitrary DT.UTCTime where
  arbitrary = DT.UTCTime <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.DateTime where
  arbitrary = B.DateTime <$> arbitrary

instance Arbitrary B.DrCr where
  arbitrary = Q.oneof [pure B.Debit, pure B.Credit]

instance Arbitrary B.Entry where
  arbitrary = B.Entry <$> arbitrary <*> arbitrary

instance Arbitrary B.Flag where
  arbitrary = B.Flag <$> arbitrary

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

instance Q.Arbitrary B.Qty where
  arbitrary = let
    p d = case B.newQty d of
      Nothing -> False
      Just _ -> True
    in B.partialNewQty <$> (Q.suchThat arbitrary p)

instance Arbitrary B.Tag where
  arbitrary = B.Tag <$> arbitrary

instance Arbitrary B.Tags where
  arbitrary = B.Tags <$> arbitrary

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

data DrCrQty = DrCrQty B.DrCr B.Qty
                 deriving (Eq, Show)

-- | Generate a random Entry, a list of Entries that, combined,
-- balance the random Entry, and (if possible) a list of Entries that
-- are in the opposite column of the random Entry but add up to
-- something less than the random Entry.
randEntries :: Gen (DrCrQty, [DrCrQty], Maybe [DrCrQty])
randEntries = let
  mkEn dc dec = DrCrQty dc (B.partialNewQty dec)
  f dc (d, ds, mds) = let
    dcOther = case dc of
      B.Debit -> B.Credit
      B.Credit -> B.Debit
    e = mkEn dc d
    es = fmap (mkEn dcOther) ds
    mes = fmap (mkEn dcOther) <$> mds
    in (e, es, mes)
  in f <$> arbitrary <*> randDecTriple
    

-- | Generate a random positive Decimal, a list of Decimals that add
-- up to that Decimal, and (if possible) a list of Decimals that add
-- up to some quantity that is less than the Decimal.
randDecTriple :: Gen (D.Decimal, [D.Decimal], Maybe [D.Decimal])
randDecTriple = do
  d <- randPosDec
  ds <- addsUpTo d
  mayBZ <- betweenZero d
  bz <- case mayBZ of
    Nothing -> return Nothing
    Just b -> Just <$> addsUpTo b
  return (d, ds, bz)
        

-- | Generate a random positive Decimal. The exponent is a Word8; this
-- function will apply fromIntegral to an Int, which should make an
-- appropriately random Word8.
randPosDec :: Gen D.Decimal
randPosDec = D.Decimal
             <$> (fromIntegral <$> (choose (0, e) :: Gen Int))
             <*> suchThat (resize size arbitrary) (>= 1)
             where
               size = 10 ^ e
               e = 10

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
addsUpTo :: D.Decimal -> Gen [D.Decimal]
addsUpTo d = (fmap unDec) <$> (addsUpToDec (Dec d))

addsUpToDec :: Dec -> Gen [Dec]
addsUpToDec t = do
  its <- suchThat arbitrary (>= 1)
  ns <- newNum t its []
  return (filter (/= (Dec (D.Decimal 0 0))) ns)
  

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
