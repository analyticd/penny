module PennyTest.Copper.Item where

import Control.Applicative ((<$>), (<*>), pure, (<*))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified PennyTest.Copper.Comments as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Item as I
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (
  Gen, oneof, Property, property,
  printTestCase)
import qualified PennyTest.Copper.Transaction as TT
import qualified PennyTest.Lincoln.Meta as TM
import qualified PennyTest.Copper.Qty as TQ
import qualified PennyTest.Copper.DateTime as TDT
import qualified PennyTest.Copper.Price as TP
import PennyTest.Copper.Util (genMaybe)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Meta as M
import qualified Text.Parsec as Parsec

-- | Generate renderable Items.
genRItem :: DT.DefaultTimeZone -> Gen I.Item
genRItem dtz = oneof [genTrans dtz, genPrice dtz, genCom, genBlank]
  
-- | Generate renderable Transactions.
genTrans :: DT.DefaultTimeZone -> Gen I.Item
genTrans dtz = do
  (transFam, metaFam) <- TT.randomRenderable dtz
  txn <- case T.transaction transFam of
    Ex.Exception e ->
      error $ "genRItem: making transaction failed: " ++ show e
    Ex.Success tr -> return tr
  let meta = M.TransactionMeta metaFam
      box = Boxes.transactionBox txn (Just meta)
  return $ I.Transaction box

-- | Generate renderable Prices.
genPrice :: DT.DefaultTimeZone -> Gen I.Item
genPrice dtz = do
  ppd <- TP.genRPricePointData dtz
  pr <- case B.newPrice (TP.from ppd)
             (TP.to ppd) (TP.countPerUnit ppd) of
          Nothing -> error $ "genRItem: making price failed"
          Just p -> return p
  let pp = B.PricePoint (TP.dateTime ppd) pr
      meta = M.PriceMeta
             <$> genMaybe TM.genPriceLine
             <*> (Just <$> TM.genFormat)
      result = Boxes.PriceBox <$> pure pp <*> (Just <$> meta)
  I.Price <$> result
  
-- | Genereate renderable Comments.
genCom :: Gen I.Item
genCom = I.Comment <$> C.genRComment

-- | Generate blank lines.
genBlank :: Gen I.Item
genBlank = return I.BlankLine

-- | Parsing a renderable Item yields the same thing.
prop_parseRenderable ::
  TDT.AnyTimeZone
  -> TM.AnyFilename
  -> TQ.AnySpecPair
  -> TQ.AnyRadGroup
  -> Property
prop_parseRenderable (TDT.AnyTimeZone dtz) afn
  (TQ.AnySpecPair gs) (TQ.AnyRadGroup rg) = do
    let TM.AnyFilename fn = afn
    i <- genRItem dtz
    case I.render dtz gs rg i of
      Nothing -> printTestCase ("render failed. Item: " ++ show i)
                 False
      Just x -> let
        parser = I.itemWithLineNumber fn dtz rg <* Parsec.eof
        in case Parsec.parse parser "" x of
          Left e -> printTestCase ("parse failed: " ++ show e)
                    False
          Right (_, i') -> if itemsEq i i'
                           then property True
                           else printTestCase "items not equal"
                                False

test_parseRenderable :: Test
test_parseRenderable = testProperty s prop_parseRenderable where
  s = "Parsing rendered Item gives same Item"

itemsEq :: I.Item -> I.Item -> Bool
itemsEq i1 i2 = case (i1, i2) of
  (I.Transaction t1, I.Transaction t2) ->
    TT.boxesEqual t1 t2
  (I.Price p1, I.Price p2) ->
    TP.pricesEqual p1 p2
  (I.Comment c1, I.Comment c2) ->
    c1 == c2
  (I.BlankLine, I.BlankLine) -> True
  _ -> False

tests :: Test
tests = testGroup "Item"
        [ test_parseRenderable ]
