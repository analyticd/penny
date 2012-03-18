module PennyTest.Copper.Price (tests) where

import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Price as P
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE
import qualified Penny.Copper.Entry as E
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import qualified PennyTest.Copper.DateTime as TDT
import PennyTest.Copper.Qty ()
import PennyTest.Copper.Util (wrapTextNonEmptyList, genTextNonEmpty)

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as Parsec
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen, listOf,
                        oneof)

data PricePointData =
  PricePointData { dateTime :: B.DateTime
                 , from :: B.From
                 , to :: B.To
                 , countPerUnit :: B.CountPerUnit
                 , defaultTimeZone :: DT.DefaultTimeZone }
  deriving (Eq, Show)

-- | Generates data to make a renderable PricePoint. The Commodities
-- are distributed evenly amongst Levels 1, 2, and 3. Some of the time
-- zones for the DateTime will be the same as the DefaultTimeZone
-- given; in addition, some will also be at midnight for the given
-- time zone.
genRPricePointData :: Gen PricePointData
genRPricePointData = do
  (dtz, dt) <- genDTZandDT
  from <- B.From <$> genRCmdty
  to <- B.To <$> suchThat genRCmdty (/= (B.unFrom from))
  cpu <- B.CountPerUnit <$> arbitrary
  return $ PricePointData dt from to cpu dtz

newtype RPricePointData = RPricePointData PricePointData
                          deriving (Eq, Show)

instance Arbitrary RPricePointData where
  arbitrary = RPricePointData <$> genRPricePointData

-- | Generates a DefaultTimeZone and a DateTime. Some of the time zones
-- for the DateTime will be the same as the DefaultTimeZone given; in
-- addition, some will also be at midnight for the given time zone.
genDTZandDT :: Gen (DT.DefaultTimeZone, B.DateTime)
genDTZandDT = oneof [random, sameZone, sameZoneMidnight] where
  random = (,) <$> arbitrary <*> arbitrary
  sameZone = (\(TDT.SameTimeZone dtz dt) -> (dtz, dt))
             <$> arbitrary
  sameZoneMidnight =
    (\(TDT.SameZoneMidnight dtz dt) -> (dtz, dt))
    <$> arbitrary

-- | Parsing a renderable PricePoint should give the same thing.
prop_parseRPricePoint ::
  RPricePointData
  -> (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> M.Format
  -> Bool
prop_parseRPricePoint (RPricePointData ppd) (gl, gr) rg fmt =
  fromMaybe False $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg fmt pp
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    box <- either (const Nothing) Just $ Parsec.parse parser "" txt
    priceMeta <- Boxes.priceMeta box
    fmt' <- M.priceFormat priceMeta
    return $ fmt' == fmt && Boxes.price box == pp

test_parseRPricePoint :: Test
test_parseRPricePoint = testProperty s prop_parseRPricePoint where
  s = "Parsing a renderable PricePoint should give the same thing"

tests :: Test
tests = testGroup "Price"
        [ test_parseRPricePoint ]
