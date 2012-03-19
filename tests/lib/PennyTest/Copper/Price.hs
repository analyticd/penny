module PennyTest.Copper.Price (tests) where

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Price as P
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import qualified PennyTest.Copper.DateTime as TDT
import PennyTest.Copper.Qty ()

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Maybe (fromMaybe)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as Parsec
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen, oneof)

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
  fr <- B.From <$> genRCmdty
  t <- B.To <$> suchThat genRCmdty (/= (B.unFrom fr))
  cpu <- B.CountPerUnit <$> arbitrary
  return $ PricePointData dt fr t cpu dtz

newtype RPricePointData = RPricePointData PricePointData
                          deriving (Eq, Show)

instance Arbitrary RPricePointData where
  arbitrary = RPricePointData <$> genRPricePointData

newtype RandomPricePointData = RandomPricePointData PricePointData
                               deriving (Eq, Show)

instance Arbitrary RandomPricePointData where
  arbitrary =
    RandomPricePointData
    <$> (PricePointData
         <$> arbitrary
         <*> (B.From <$> arbitrary)
         <*> (B.To <$> arbitrary)
         <*> (B.CountPerUnit <$> arbitrary)
         <*> arbitrary )

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

-- | Random, unrenderable PricePointData makes a valid Price.
prop_randomIsValid :: RandomPricePointData -> Bool
prop_randomIsValid (RandomPricePointData ppd) =
  maybe False (const True) $
  B.newPrice (from ppd) (to ppd) (countPerUnit ppd)

test_randomIsValid :: Test
test_randomIsValid = testProperty s prop_randomIsValid where
  s = "random unrenderable PricePointData is valid Price"

-- | Renderable PricePointData makes a valid Price.
prop_renderableIsValid :: RPricePointData -> Bool
prop_renderableIsValid (RPricePointData ppd) =
  maybe False (const True) $
  B.newPrice (from ppd) (to ppd) (countPerUnit ppd)

test_renderableIsValid :: Test
test_renderableIsValid = testProperty s prop_renderableIsValid where
  s = "renderable PricePointData is valid Price"

-- | Renderable PricePointData is renderable
prop_isRenderable ::
  RPricePointData
  -> (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> M.Format
  -> Bool
prop_isRenderable (RPricePointData ppd) (gl, gr) rg fmt =
  fromMaybe False $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    _ <- P.render (defaultTimeZone ppd) gl gr rg fmt pp
    return True

test_isRenderable :: Test
test_isRenderable = testProperty s prop_isRenderable where
  s = "renderable PricePoint data is indeed renderable"

-- | Renderable PricePoint parses without error
prop_renderableParses ::
  RPricePointData
  -> (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> M.Format
  -> Bool
prop_renderableParses (RPricePointData ppd) (gl, gr) rg fmt =
  fromMaybe False $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg fmt pp
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    case Parsec.parse parser "" txt of
      Left e -> error $ "parse error: " ++ show e
                ++ "rendered: " ++ show txt
      Right _ -> return True

test_renderableParses :: Test
test_renderableParses = testProperty s prop_renderableParses where
  s = "A renderable PricePoint parses without error"

-- | Parsing a renderable PricePoint should give the same price.
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
    if Boxes.price box /= pp
       then error $ "\nparsed price point differs. Input: "
            ++ show pp
            ++ "\nparsed: " ++ show (Boxes.price box)
            ++ "\nRendered: " ++ show txt
      else return True

test_parseRPricePoint :: Test
test_parseRPricePoint = testProperty s prop_parseRPricePoint where
  s = "Parsing a renderable PricePoint should give the same PricePoint"

-- | Parsing a renderable PricePoint should give the same price and
-- metadata
prop_parseRPricePointMeta ::
  RPricePointData
  -> (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> M.Format
  -> Bool
prop_parseRPricePointMeta (RPricePointData ppd) (gl, gr) rg fmt =
  fromMaybe False $ do
    p <- B.newPrice (from ppd) (to ppd) (countPerUnit ppd)
    let pp = B.PricePoint (dateTime ppd) p
    txt <- P.render (defaultTimeZone ppd) gl gr rg fmt pp
    let parser = P.price (defaultTimeZone ppd) rg <* Parsec.eof
    box <- either (const Nothing) Just $ Parsec.parse parser "" txt
    priceMeta <- Boxes.priceMeta box
    fmt' <- M.priceFormat priceMeta
    return $ fmt' == fmt && Boxes.price box == pp

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
