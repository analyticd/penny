module PennyTest.Lincoln.Bits where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Decimal as D
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromJust)
import qualified Data.Time as DT
import Data.Word (Word8)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Bits as B
import PennyTest.Lincoln.TextNonEmpty (genTextNonEmpty)
import qualified System.Random as R
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, Arbitrary, Gen, choose, suchThat,
                        sized, oneof, listOf, elements, frequency)

-- | Generates Unicode characters over the entire Unicode
-- range. Characters in ASCII and in Latin1 are heavily favored;
-- however, all characters are generated.
unicode :: Gen Char
unicode = oneof [ ascii, latin1, others ] where
  ascii = toEnum <$> choose (0, 127)
  latin1 = toEnum <$> choose (128, 255)
  others = toEnum <$> choose (256, fromEnum (maxBound :: Char))

-- | Generates SubAccountNames from characters throughout the Unicode
-- range.
genUniSubAccountName :: Gen B.SubAccountName
genUniSubAccountName =
  B.SubAccountName
  <$> (genTextNonEmpty unicode unicode)

-- | Generates a NonEmptyList using the generators for the first
-- element and for all subsequent elements. The length of the tail
-- depends upon the size parameter.
genNonEmpty :: Gen a -> Gen a -> Gen (NE.NonEmpty a)
genNonEmpty g1 gr =
  (:|)
  <$> g1
  <*> listOf gr

-- | Generates Accounts from characters throughout the Unicode range.
genUniAccount :: Gen B.Account
genUniAccount = B.Account <$> genNonEmpty g g where
  g = genUniSubAccountName

genUniAmount :: Gen B.Amount
genUniAmount = B.Amount <$> genQty <*> genUniCommodity 

-- | Generate a SubCommodity from characters through the Unicode
-- range.
genUniSubCommodity :: Gen B.SubCommodity
genUniSubCommodity = B.SubCommodity
                     <$> genTextNonEmpty unicode unicode

-- | Generate a Commodity from characters throughout the Unicode
-- range.

genUniCommodity :: Gen B.Commodity
genUniCommodity = B.Commodity <$> genNonEmpty g g where
  g = genUniSubCommodity

-- | Generate a DiffTime that is in the range [0, 86400) seconds.
genDiffTime :: Gen DT.DiffTime
genDiffTime = DT.secondsToDiffTime
              <$> (Q.suchThat arbitrary (\s -> s >= 0 && s < 86400))

newtype DiffTime = DiffTime DT.DiffTime
                       deriving (Eq, Show)
instance Q.Arbitrary DiffTime where
  arbitrary = DiffTime <$> genDiffTime

-- | Generate a Day that falls between the years 1858 and 2132.
genDay :: Gen DT.Day
genDay = DT.ModifiedJulianDay <$> choose (0, 100000)

newtype Day = Day DT.Day
                  deriving (Eq, Show)
instance Q.Arbitrary Day where
  arbitrary = Day <$> genDay

-- | Generate a UTC time using genDay and genDiffTime.
genUTCTime :: Gen DT.UTCTime
genUTCTime = DT.UTCTime <$> genDay <*> genDiffTime

newtype UTCTime = UTCTime DT.UTCTime
                      deriving (Eq, Show)
instance Q.Arbitrary UTCTime where
  arbitrary = UTCTime <$> genUTCTime

-- | Generate a TimeOfDay whose values are valid.
genTimeOfDay :: Gen DT.TimeOfDay
genTimeOfDay =
  DT.TimeOfDay
  <$> choose (0, 23)
  <*> choose (0, 59)
  <*> (fromIntegral <$> choose (0 :: Int, 59))

newtype TimeOfDay = TimeOfDay DT.TimeOfDay
                        deriving (Eq, Show)
instance Q.Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> genTimeOfDay

-- | Generate a LocalTime using genDay and genTimeOfDay.
genLocalTime :: Gen DT.LocalTime
genLocalTime =
  DT.LocalTime <$> genDay <*> genTimeOfDay

newtype LocalTime = LocalTime DT.LocalTime
                        deriving (Eq, Show)
instance Q.Arbitrary LocalTime where
  arbitrary = LocalTime <$> genLocalTime

-- | Generate a TimeZoneOffset between -840 and 840 minutes.
genTimeZoneOffset :: Gen B.TimeZoneOffset
genTimeZoneOffset = do
  i <- choose ((-840), 840)
  maybe (error "arbitrary TimeZoneOffset failed") return
    $ B.minsToOffset i
  
newtype TimeZoneOffset =
  TimeZoneOffset B.TimeZoneOffset
  deriving (Eq, Show)
instance Arbitrary TimeZoneOffset where
  arbitrary = TimeZoneOffset <$> genTimeZoneOffset

-- | Generate a DateTime using genLocalTime and genTimeZoneOffset.
genDateTime :: Gen B.DateTime
genDateTime = B.dateTime <$> genLocalTime <*> genTimeZoneOffset

newtype DateTime = DateTime B.DateTime
                       deriving (Eq, Show)
instance Arbitrary DateTime where
  arbitrary = DateTime <$> genDateTime

genDrCr :: Gen B.DrCr
genDrCr = elements [B.Debit, B.Credit]

genUniEntry :: Gen B.Entry
genUniEntry = B.Entry <$> genDrCr <*> genUniAmount

genUniFlag :: Gen B.Flag
genUniFlag = B.Flag <$> genTextNonEmpty unicode unicode

genUniMemoLine :: Gen B.MemoLine
genUniMemoLine = B.MemoLine <$> genTextNonEmpty unicode unicode

genUniMemo :: Gen B.Memo
genUniMemo = B.Memo <$> listOf g where
  g = genUniMemoLine
  
genUniNumber :: Gen B.Number
genUniNumber = B.Number <$> genTextNonEmpty unicode unicode

genUniPayee :: Gen B.Payee
genUniPayee = B.Payee <$> genTextNonEmpty unicode unicode

genUniFrom :: Gen B.From
genUniFrom = B.From <$> genUniCommodity

genUniTo :: Gen B.To
genUniTo = B.To <$> genUniCommodity

genCountPerUnit :: Gen B.CountPerUnit
genCountPerUnit = B.CountPerUnit <$> genQty

genUniPrice :: Gen B.Price
genUniPrice = mkPrice <$> Q.suchThat g p where
    g = (,,) <$> genUniFrom <*> genUniTo <*> genCountPerUnit
    p (f, t, c) = case B.newPrice f t c of
      Nothing -> False
      Just _ -> True
    mkPrice (f, t, c) = fromJust $ B.newPrice f t c

genPriceLine :: Gen L.PriceLine
genPriceLine = L.PriceLine <$> arbitrary

genSide :: Gen L.Side
genSide = elements [L.CommodityOnLeft, L.CommodityOnRight]

genSpaceBetween :: Gen L.SpaceBetween
genSpaceBetween = elements [L.SpaceBetween, L.NoSpaceBetween]

genFormat :: Gen L.Format
genFormat = L.Format <$> genSide <*> genSpaceBetween

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = frequency [ (3, Just <$> g)
                       , (1, return Nothing) ]

genPriceMeta :: Gen L.PriceMeta
genPriceMeta = L.PriceMeta
               <$> genMaybe genPriceLine
               <*> genMaybe genFormat


genPrice :: Gen L.Price
genPrice = do
  cpu <- genCountPerUnit
  f <- L.From <$> genUniCommodity
  t <- L.To <$> genUniCommodity
  case L.newPrice f t cpu of
    Nothing -> genPrice
    Just p -> return p

genPricePoint :: Gen L.PricePoint
genPricePoint =
  L.PricePoint <$> genDateTime <*> genPrice <*> genPriceMeta


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

genQty :: Gen B.Qty
genQty = B.partialNewQty <$> randomDecimal

genUniTag :: Gen B.Tag
genUniTag = B.Tag <$> genTextNonEmpty unicode unicode

genUniTags :: Gen B.Tags
genUniTags = B.Tags <$> listOf genUniTag

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
  in f <$> genDrCr <*> randDecTriple
    
    
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


