module PennyTest.Copper.DateTime (
  SameTimeZone(SameTimeZone)
  , SameZoneMidnight (SameZoneMidnight)
  , tests
  ) where

import Control.Applicative ((<$>), (<*))
import qualified Data.Time as T
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary)

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Bits as B

-- | The time zone of the DateTime and the default time zone are the
-- same. The time is otherwise random.
data SameTimeZone =
  SameTimeZone DT.DefaultTimeZone B.DateTime
  deriving (Show, Eq)

instance Arbitrary SameTimeZone where
  arbitrary = do
    offset <- arbitrary
    lt <- arbitrary
    let dtz = DT.DefaultTimeZone offset
        dt = B.DateTime lt offset
    return $ SameTimeZone dtz dt

instance Arbitrary DT.DefaultTimeZone where
  arbitrary = DT.DefaultTimeZone <$> arbitrary

-- | The time zone is the same and the time is midnight.
data SameZoneMidnight =
  SameZoneMidnight DT.DefaultTimeZone B.DateTime
  deriving (Show, Eq)

instance Arbitrary SameZoneMidnight where
  arbitrary = do
    offset <- arbitrary
    day <- arbitrary
    let dtz = DT.DefaultTimeZone offset
        lt = T.LocalTime day T.midnight
        dt = B.DateTime lt offset
    return $ SameZoneMidnight dtz dt

-- | Parsing a random rendered DateTime should yield the same thing.
prop_parseRendered :: DT.DefaultTimeZone -> B.DateTime -> Bool
prop_parseRendered dtz dt =
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
prop_parseSameTimeZone :: SameTimeZone -> Bool
prop_parseSameTimeZone (SameTimeZone dtz dt) = let
  r = DT.render dtz dt
  p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right dt' -> dt == dt'

test_parseSameTimeZone :: Test
test_parseSameTimeZone = testProperty s prop_parseSameTimeZone where
  s = "Parsing DateTime from same time zone "
      ++ "as default yields same DateTime"


-- | Parsing rendered where the time is midnight gives a successful
-- parse.
prop_midnightParses :: SameZoneMidnight -> Bool
prop_midnightParses szm = let
  (SameZoneMidnight dtz dt) = szm
  r = DT.render dtz dt
  p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right _ -> True

test_midnightParses :: Test
test_midnightParses =
  testProperty s prop_midnightParses where
    s = "Parsing midnight parses successfully"

-- | Parsing rendered where the time is midnight should yield the same
-- local time.
prop_parseMidnightLocalTime :: SameZoneMidnight -> Bool
prop_parseMidnightLocalTime szm = let
  (SameZoneMidnight dtz dt) = szm
  r = DT.render dtz dt
  p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right dt' -> dt == dt'

test_parseMidnightLocalTime :: Test
test_parseMidnightLocalTime =
  testProperty s prop_parseMidnightLocalTime where
    s = "Parsing midnight yields same local time"

-- | Parsing a rendered DateTime where the time zone is the same as
-- the DefaultTimeZone and the time is midnight should yield the same
-- thing.
prop_parseMidnight :: SameZoneMidnight -> Bool
prop_parseMidnight (SameZoneMidnight dtz dt) = let
  r = DT.render dtz dt
  p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right dt' -> dt == dt'

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
