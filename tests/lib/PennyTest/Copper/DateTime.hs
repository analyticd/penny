module PennyTest.Copper.DateTime where

import Control.Applicative ((<$>), (<*))
import qualified Data.Text as X
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

-- | The time zone of the DateTime and the default time zone are the
-- same. However, the time is otherwise random.

-- | Parsing a random rendered DateTime should yield the same thing.
prop_parseRendered :: DT.DefaultTimeZone -> B.DateTime -> Bool
prop_parseRendered = undefined

test_parseRendered :: Test
test_parseRendered = undefined

tests :: Test
tests = testGroup "DateTime"
        [ test_parseRendered ]
