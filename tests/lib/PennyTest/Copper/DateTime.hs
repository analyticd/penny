module PennyTest.Copper.DateTime where

import Control.Applicative ((<$>), (<*))
import qualified Data.Text as X
import qualified Data.Time as T
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (
  Arbitrary, arbitrary, suchThat, Gen,
  frequency, listOf, listOf1, sized, resize, oneof,
  vectorOf)

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Bits as B

-- | The time zone of the DateTime and the default time zone are the
-- same. The time is otherwise random.
data SameTimeZone =
  SameTimeZone DT.DefaultTimeZone B.DateTime
  deriving (Show, Eq)

instance Arbitrary DT.DefaultTimeZone where
  arbitrary = DT.DefaultTimeZone <$> arbitrary

-- | Tests ZonedTimes to see if they are equivalent. Only examines the
-- LocalTime and the timeZoneMinutes of the TimeZone; ignores other
-- aspects of the time zone.
sameTimeZone :: T.TimeZone -> T.TimeZone -> Bool
sameTimeZone t1 t2 =
  T.timeZoneMinutes t1 == T.timeZoneMinutes t2

-- | Parsing a random rendered DateTime should yield the same thing.
prop_parseRendered :: DT.DefaultTimeZone -> B.DateTime -> Bool
prop_parseRendered dtz dt@(B.DateTime zt) =
  let r = DT.render dtz dt
      p = P.parse (DT.dateTime dtz <* P.eof) "" r
  in case p of
    Left _ -> False
    Right (B.DateTime zt') ->
      (T.zonedTimeToLocalTime zt == T.zonedTimeToLocalTime zt')
      && sameTimeZone (T.zonedTimeZone zt) (T.zonedTimeZone zt')

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing random rendered DateTime yields same DateTime"

tests :: Test
tests = testGroup "DateTime"
        [ test_parseRendered ]
