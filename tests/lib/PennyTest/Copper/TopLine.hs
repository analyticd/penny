module PennyTest.Copper.TopLine where

import Control.Applicative ((<$>), (<*>), (<*))
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.TopLine as T
import Test.QuickCheck (Arbitrary, arbitrary, Gen, oneof)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
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

-- | Parsing a rendered TopLine should yield the same thing.
prop_parseRendered :: RTopLine -> Bool
prop_parseRendered (RTopLine (tl, dtz)) =
  case T.render dtz tl of
    Nothing -> error "Not renderable"
    Just x -> case P.parse (T.topLine dtz <* P.eof) "" x of
      Left e -> error $ "parse failed: " ++ show e
                ++ " rendered: " ++ show x
      Right (tl', _, _) -> tl' == tl

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "parsing rendered TopLine yields same thing"

tests :: Test
tests = testGroup "TopLine"
        [ test_parseRendered ]
