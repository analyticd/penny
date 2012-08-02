-- | Generators for all the data types in Lincoln. Also contains some
-- utility functions that may be useful elsewhere.

module PennyTest.Lincoln.Generators where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Decimal as D
import qualified Data.Fixed as Fixed
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Time as DT
import Data.Word (Word8)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Bits as B
import PennyTest.Lincoln.TextNonEmpty (genTextNonEmpty)
import qualified System.Random as R
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, Arbitrary, Gen, choose, suchThat,
                        sized, oneof, listOf, elements, frequency)

-- * Utility functions

-- | Generates Unicode characters over the entire Unicode
-- range. Characters in ASCII and in Latin1 are heavily favored;
-- however, all characters are generated.
unicode :: Gen Char
unicode = oneof [ ascii, latin1, others ] where
  ascii = toEnum <$> choose (0, 127)
  latin1 = toEnum <$> choose (128, 255)
  others = toEnum <$> choose (256, fromEnum (maxBound :: Char))

-- | Generates a NonEmptyList using the generators for the first
-- element and for all subsequent elements. The length of the tail
-- depends upon the size parameter.
genNonEmpty :: Gen a -> Gen a -> Gen (NE.NonEmpty a)
genNonEmpty g1 gr =
  (:|)
  <$> g1
  <*> listOf gr

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = frequency [ (3, Just <$> g)
                       , (1, return Nothing) ]

-- * Bits

-- ** Accounts

-- | Generates SubAccountNames from characters throughout the Unicode
-- range.
subAccountName :: Gen B.SubAccountName
subAccountName =
  B.SubAccountName
  <$> (genTextNonEmpty unicode unicode)

-- | Generates Accounts from characters throughout the Unicode range.
account :: Gen B.Account
account = B.Account <$> genNonEmpty g g where
  g = subAccountName

-- ** Amounts

amount :: L.Qty -> Gen B.Amount
amount q = B.Amount q <$> commodity 

-- ** Commodities

-- | Generate a SubCommodity from characters through the Unicode
-- range.
subCommodity :: Gen B.SubCommodity
subCommodity = B.SubCommodity
                     <$> genTextNonEmpty unicode unicode

-- | Generate a Commodity from characters throughout the Unicode
-- range.
commodity :: Gen B.Commodity
commodity = B.Commodity <$> genNonEmpty g g where
  g = subCommodity

-- ** DateTime

-- | Generate a Day that falls between the years 1858 and 2132.
day :: Gen DT.Day
day = DT.ModifiedJulianDay <$> choose (0, 100000)

-- | Generate a TimeOfDay.
timeOfDay :: Gen DT.TimeOfDay
timeOfDay =
  DT.TimeOfDay
  <$> choose (0, 23)
  <*> choose (0, 59)
  <*> pico

-- | Generate a Pico in the range [0, 61).
pico :: Gen Fixed.Pico
pico =
  fromRational . toRational
  <$> suchThat (choose (0, 61)) (< (61.0 :: Double))


-- | Generate a valid LocalTime.
localTime :: Gen DT.LocalTime
localTime = DT.LocalTime <$> day <*> timeOfDay


-- | Pass in a generator for integers. If the generator returns a
-- number in the range [-840, 840], this should return a
-- TimeZoneOffset. Otherwise, this should fail.
timeZoneOffset ::
  Gen Int
  -> Gen (Maybe L.TimeZoneOffset)
timeZoneOffset = fmap L.minsToOffset

-- | Pass in the time zone offset; if an offset is
-- successfully generated, return a DateTime.
dateTime :: L.TimeZoneOffset -> Gen L.DateTime
dateTime tzo = flip L.dateTime tzo <$> localTime


-- ** Debits and credits

drCr :: Gen B.DrCr
drCr = elements [B.Debit, B.Credit]

-- ** Entries

entry :: L.Qty -> Gen B.Entry
entry q = B.Entry <$> drCr <*> (amount q)

-- ** Flag

flag :: Gen B.Flag
flag = B.Flag <$> genTextNonEmpty unicode unicode

-- ** Memo

memoLine :: Gen B.MemoLine
memoLine = B.MemoLine <$> genTextNonEmpty unicode unicode

memo :: Gen B.Memo
memo = B.Memo <$> listOf g where
  g = memoLine
  
-- ** Number

number :: Gen B.Number
number = B.Number <$> genTextNonEmpty unicode unicode

-- ** Payee

payee :: Gen B.Payee
payee = B.Payee <$> genTextNonEmpty unicode unicode

-- ** Price and price points

from :: Gen B.From
from = B.From <$> commodity

to :: Gen B.To
to = B.To <$> commodity

priceLine :: Gen L.PriceLine
priceLine = L.PriceLine <$> arbitrary

side :: Gen L.Side
side = elements [L.CommodityOnLeft, L.CommodityOnRight]

spaceBetween :: Gen L.SpaceBetween
spaceBetween = elements [L.SpaceBetween, L.NoSpaceBetween]

format :: Gen L.Format
format = L.Format <$> side <*> spaceBetween

priceMeta :: Gen L.PriceMeta
priceMeta = L.PriceMeta
            <$> genMaybe priceLine
            <*> genMaybe format


-- | When given a Price, generate a PricePoint.
pricePoint :: L.TimeZoneOffset -> L.Price -> Gen L.PricePoint
pricePoint tzo p =
  (\dt pm -> L.PricePoint dt p pm)
  <$> dateTime tzo
  <*> priceMeta


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

-- ** Tags

tag :: Gen B.Tag
tag = B.Tag <$> genTextNonEmpty unicode unicode

tags :: Gen B.Tags
tags = B.Tags <$> listOf tag

-- * Metadata

-- | Generates Ints over the entire range of Int, but generates zeroes
-- about a fourth of the time. The other three-fourths of values are
-- randomly distributed over the range of Int. Does not depend on the
-- size parameter.

-- * Entry generators

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
  in f <$> drCr <*> randDecTriple


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


