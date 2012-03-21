module PennyTest.Copper.Price (
  tests, genDT,
  PricePointData(PricePointData, dateTime, from,
                 to, countPerUnit, defaultTimeZone),
  genRPricePointData,
  pricesEqual
  ) where

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Price as P
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M

import qualified PennyTest.Lincoln.Bits as TB
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import qualified PennyTest.Copper.DateTime as TDT
import PennyTest.Copper.Qty ()

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Maybe (fromMaybe)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as Parsec
import Test.Framework (Test, testGroup)
import Test.QuickCheck (
  Arbitrary, arbitrary, suchThat, Gen, oneof,
  property, Property, printTestCase)

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
genRPricePointData :: DT.DefaultTimeZone -> Gen PricePointData
genRPricePointData dtz = do
  dt <- genDT dtz
  fr <- B.From <$> genRCmdty
  t <- B.To <$> suchThat genRCmdty (/= (B.unFrom fr))
  cpu <- TB.genCountPerUnit
  return $ PricePointData dt fr t cpu dtz

newtype RandomPricePointData = RandomPricePointData PricePointData
                               deriving (Eq, Show)

instance Arbitrary RandomPricePointData where
  arbitrary =
    RandomPricePointData
    <$> (PricePointData
         <$> TB.genDateTime
         <*> TB.genUniFrom
         <*> TB.genUniTo
         <*> TB.genCountPerUnit
         <*> TDT.genAnyTimeZone )

-- | Checks to see if two PriceBoxes are equal.
pricesEqual :: Boxes.PriceBox -> Boxes.PriceBox -> Bool
pricesEqual b1 b2 = maybe False id $ do
  let p1 = Boxes.price b1
      p2 = Boxes.price b2
  m1 <- Boxes.priceMeta b1
  m2 <- Boxes.priceMeta b2
  f1 <- M.priceFormat m1
  f2 <- M.priceFormat m2
  return ((p1 == p2) && (f1 == f2))

-- | Generates a DateTime. Some of the time zones
-- for the DateTime will be the same as the DefaultTimeZone given; in
-- addition, some will also be at midnight for the given time zone.
genDT :: DT.DefaultTimeZone -> Gen B.DateTime
genDT dtz = oneof [ TB.genDateTime
                  , TDT.genSameTimeZone dtz
                  , TDT.genSameZoneMidnight dtz ]

-- | Random, unrenderable PricePointData makes a valid Price.
prop_randomIsValid :: RandomPricePointData -> Bool
prop_randomIsValid (RandomPricePointData ppd) =
  maybe False (const True) $
  B.newPrice (from ppd) (to ppd) (countPerUnit ppd)

test_randomIsValid :: Test
test_randomIsValid = testProperty s prop_randomIsValid where
  s = "random unrenderable PricePointData is valid Price"

-- | Renderable PricePointData makes a valid Price.
prop_renderableIsValid :: Property
prop_renderableIsValid = do
  dtz <- TDT.genAnyTimeZone
  ppd <- genRPricePointData dtz
  maybe (property False) (const (property True)) $
    B.newPrice (from ppd) (to ppd) (countPerUnit ppd)

test_renderableIsValid :: Test
test_renderableIsValid = testProperty s prop_renderableIsValid where
  s = "renderable PricePointData is valid Price"

-- | Renderable PricePointData is renderable
prop_isRenderable :: Property
prop_isRenderable = do
  dtz <- TDT.genAnyTimeZone
  ppd <- genRPricePointData dtz
  (gl, gr, rg, fmt) <- arbitrary
  fromMaybe (property False) $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    _ <- P.render (defaultTimeZone ppd) gl gr rg (pp, fmt)
    return (property True)
  
test_isRenderable :: Test
test_isRenderable = testProperty s prop_isRenderable where
  s = "renderable PricePoint data is indeed renderable"

-- | Renderable PricePoint parses without error
prop_renderableParses :: Property
prop_renderableParses = do
  (gl, gr, rg, fmt) <- arbitrary
  dtz <- TDT.genAnyTimeZone
  ppd <- genRPricePointData dtz
  fromMaybe (property False) $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg (pp, fmt)
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    case Parsec.parse parser "" txt of
      Left e -> let
        msg = "parse error: " ++ show e
              ++ " rendered: " ++ show txt
        in return $ printTestCase msg False
      Right _ -> return $ property True

test_renderableParses :: Test
test_renderableParses = testProperty s prop_renderableParses where
  s = "A renderable PricePoint parses without error"

-- | Parsing a renderable PricePoint should give the same price.
prop_parseRPricePoint :: Property
prop_parseRPricePoint = do
  (gl, gr, rg, fmt) <- arbitrary
  dtz <- TDT.genAnyTimeZone
  ppd <- genRPricePointData dtz
  fromMaybe (property False) $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg (pp, fmt)
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    box <- either (const Nothing) Just $ Parsec.parse parser "" txt
    if Boxes.price box /= pp
       then
       let msg = "\nparsed price point differs. Input: "
                 ++ show pp
                 ++ "\nparsed: " ++ show (Boxes.price box)
                 ++ "\nRendered: " ++ show txt
       in return $ printTestCase msg False
      else return $ property True

test_parseRPricePoint :: Test
test_parseRPricePoint = testProperty s prop_parseRPricePoint where
  s = "Parsing a renderable PricePoint should give the same PricePoint"

-- | Parsing a renderable PricePoint should give the same price and
-- metadata
prop_parseRPricePointMeta :: Property
prop_parseRPricePointMeta = do
  (gl, gr, rg, fmt) <- arbitrary
  dtz <- TDT.genAnyTimeZone
  ppd <- genRPricePointData dtz
  fromMaybe (property False) $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg (pp, fmt)
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    box <- either (const Nothing) Just $ Parsec.parse parser "" txt
    priceMeta <- Boxes.priceMeta box
    fmt' <- M.priceFormat priceMeta
    return (property (fmt' == fmt && Boxes.price box == pp))

test_parseRPricePointMeta :: Test
test_parseRPricePointMeta =
  testProperty s prop_parseRPricePointMeta where
    s = "Parsing renderable PricePoint gives same price and metadata"

tests :: Test
tests = testGroup "Price"
        [ test_randomIsValid
          , test_renderableIsValid
          , test_isRenderable
          , test_renderableParses
          , test_parseRPricePoint
          , test_parseRPricePointMeta ]
