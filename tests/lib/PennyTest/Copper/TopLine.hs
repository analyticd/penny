module PennyTest.Copper.TopLine where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Copper.DateTime as DT
import Test.QuickCheck (Arbitrary, arbitrary, Gen, oneof)
import Test.Framework (Test, testGroup)
import qualified Penny.Lincoln.Transaction.Unverified as U

import PennyTest.Copper.Price (genDTZandDT)
import qualified PennyTest.Copper.Flag as Flag
import qualified PennyTest.Copper.Number as Number
import qualified PennyTest.Copper.Payees as Payees
import qualified PennyTest.Copper.Memos.Transaction as Memos
import PennyTest.Copper.Util (genMaybe)

-- | Generate renderable TopLine and a DefaultTimeZone. Some of the
-- DefaultTimeZones will be in the same time zone as the DateTime; in
-- addition, some will also be at midnight and be at the same time
-- zone.
genRTopLine :: Gen (U.TopLine, DT.DefaultTimeZone)
genRTopLine =
  f
  <$> genDTZandDT
  <*> genMaybe Flag.genRFlag
  <*> genMaybe Number.genRNumber
  <*> genMaybe (oneof [Payees.genNoQuotePayee
                      , Payees.genNeedsQuotePayee])
  <*> Memos.genRMemo
  where
    f (dtz, dt) fl nu pa me = (U.TopLine dt fl nu pa me, dtz)

newtype RTopLine = RTopLine (U.TopLine, DT.DefaultTimeZone)
                   deriving (Show, Eq)
instance Arbitrary RTopLine where
  arbitrary = RTopLine <$> genRTopLine

tests :: Test
tests = testGroup "TopLine" []

