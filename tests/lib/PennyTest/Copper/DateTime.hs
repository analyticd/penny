module PennyTest.Copper.DateTime (
  genSameTimeZone
  , genSameZoneMidnight
  , genAnyTimeZone
  , AnyTimeZone(AnyTimeZone)
  , tests
  ) where

import Control.Applicative ((<$>), (<*), (<*>), pure)
import qualified Data.Time as T
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (
  Arbitrary, arbitrary, Gen, Property, property)

-- Import orphan instances of Arbitrary
import qualified PennyTest.Lincoln.Bits as TB

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Bits as B

genSameTimeZone :: DT.DefaultTimeZone -> Gen B.DateTime
genSameTimeZone (DT.DefaultTimeZone offset) =
  B.DateTime <$> TB.genLocalTime <*> pure offset

genSameZoneMidnight :: DT.DefaultTimeZone -> Gen B.DateTime
genSameZoneMidnight (DT.DefaultTimeZone offset) =
  B.DateTime
  <$> (T.LocalTime <$> TB.genDay <*> pure T.midnight)
  <*> pure offset

genAnyTimeZone :: Gen DT.DefaultTimeZone
genAnyTimeZone = DT.DefaultTimeZone <$> TB.genTimeZoneOffset

newtype AnyTimeZone = AnyTimeZone DT.DefaultTimeZone
                      deriving (Eq, Show)
instance Arbitrary AnyTimeZone where
  arbitrary = AnyTimeZone <$> genAnyTimeZone

-- | Parsing a random rendered DateTime should yield the same thing.
prop_parseRendered :: AnyTimeZone -> TB.DateTime -> Bool
prop_parseRendered (AnyTimeZone dtz) (TB.DateTime dt) =
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right dt' -> dt == dt'

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing random rendered DateTime yields same DateTime"

-- | Parsing a rendered DateTime where the time zone is the same as
-- the DefaultTimeZone should yield the same thing.
prop_parseSameTimeZone :: AnyTimeZone -> Property
prop_parseSameTimeZone (AnyTimeZone dtz) = do
  dt <- genSameTimeZone dtz
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  case p of
    Left _ -> property False
    Right dt' -> property (dt == dt')

test_parseSameTimeZone :: Test
test_parseSameTimeZone = testProperty s prop_parseSameTimeZone where
  s = "Parsing DateTime from same time zone "
      ++ "as default yields same DateTime"


-- | Parsing rendered where the time is midnight gives a successful
-- parse.
prop_midnightParses :: AnyTimeZone -> Property
prop_midnightParses (AnyTimeZone dtz) = do 
  dt <- genSameZoneMidnight dtz
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  case p of
    Left _ -> property False
    Right _ -> property True

test_midnightParses :: Test
test_midnightParses =
  testProperty s prop_midnightParses where
    s = "Parsing midnight parses successfully"

-- | Parsing rendered where the time is midnight should yield the same
-- local time.
prop_parseMidnightLocalTime :: AnyTimeZone -> Property
prop_parseMidnightLocalTime (AnyTimeZone dtz) = do
  dt <- genSameTimeZone dtz
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  case p of
    Left _ -> property False
    Right dt' -> property $ dt == dt'

test_parseMidnightLocalTime :: Test
test_parseMidnightLocalTime =
  testProperty s prop_parseMidnightLocalTime where
    s = "Parsing midnight yields same local time"

-- | Parsing a rendered DateTime where the time zone is the same as
-- the DefaultTimeZone and the time is midnight should yield the same
-- thing.
prop_parseMidnight :: AnyTimeZone -> Property
prop_parseMidnight (AnyTimeZone dtz) = do
  dt <- genSameZoneMidnight dtz
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  case p of
    Left _ -> property False
    Right dt' -> property $ dt == dt'

test_parseMidnight :: Test
test_parseMidnight = testProperty s prop_parseMidnight where
  s = "Parsing midnight yields same DateTime"

tests :: Test
tests = testGroup "DateTime"
        [ test_parseRendered 
        , test_parseSameTimeZone 
        , test_midnightParses
        , test_parseMidnightLocalTime
        , test_parseMidnight ]
