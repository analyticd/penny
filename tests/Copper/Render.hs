{-# OPTIONS_GHC -fno-warn-missing-signatures
                -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Copper.Render where

import Control.Applicative ((<*), (<$>), (<*>))
import qualified Copper.Gen.Parsers as G
import qualified Penny.Copper.Interface as I
import qualified Penny.Copper.Render as R
import qualified Penny.Copper.Parsec as P
import qualified Penny.Lincoln as L
import qualified Text.Parsec as Ps
import qualified Text.Parsec.Text as Ps
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.All as A
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck (Gen, arbitrary, Arbitrary)
import Data.Text (Text)

renParse
  :: (Eq a, Show a)
  => (a -> Maybe Text)
  -> Ps.Parser a
  -> Gen (a, b)
  -> Q.Property
renParse r p g = Q.forAll (fmap fst g) $ \ a ->
  doParse r p a

doParseWithPdct
  :: Show a
  => (a -> a -> Bool)
  -> (a -> Maybe Text)
  -> Ps.Parser a
  -> a
  -> QCP.Result
doParseWithPdct pdct r p a =
  case r a of
    Nothing -> QCP.failed { QCP.reason = "render failed: "
                            ++ show a }
    Just rend -> case Ps.parse (p <* Ps.eof) "" rend of
      Left e -> QCP.failed { QCP.reason = "parse failed: "
                             ++ show e }
      Right g ->
        if pdct g a
        then QCP.succeeded
        else QCP.failed
             { QCP.reason = "parsed not equal to original. "
               ++ "Original: " ++ show a ++ " parsed: "
               ++ show g }

doParse
  :: (Eq a, Show a)
  => (a -> Maybe Text)
  -> Ps.Parser a
  -> a
  -> QCP.Result
doParse = doParseWithPdct (==)

prop_quotedLvl1Acct =
  renParse R.quotedLvl1Acct P.quotedLvl1Acct G.quotedLvl1Acct

prop_lvl2Acct =
  renParse R.lvl2Acct P.lvl2Acct G.lvl2Acct

prop_ledgerAcct =
  renParse R.ledgerAcct P.ledgerAcct G.ledgerAcct

prop_quotedLvl1Cmdty =
  renParse R.quotedLvl1Cmdty P.quotedLvl1Cmdty
  (fmap (\ (G.QuotedLvl1Cmdty c _) -> (c, ())) G.quotedLvl1Cmdty)

prop_lvl2Cmdty =
  renParse R.lvl2Cmdty P.lvl2Cmdty
  (fmap (\(G.Lvl2Cmdty c _) -> (c, ())) G.lvl2Cmdty)

prop_lvl3Cmdty =
  renParse R.lvl3Cmdty P.lvl3Cmdty
  (fmap (\(G.Lvl3Cmdty c _) -> (c, ())) G.lvl3Cmdty)

instance Arbitrary R.GroupSpec where
  arbitrary = Q.elements [R.NoGrouping, R.GroupLarge, R.GroupAll]

instance Arbitrary R.GroupSpecs where
  arbitrary = R.GroupSpecs <$> arbitrary <*> arbitrary

prop_quantity = do
  gs <- arbitrary
  (q, _) <- G.quantity
  return $ doParse (fmap Just (R.quantity gs)) P.quantity q

prop_amount = do
  gs <- arbitrary
  cy <- G.genCmdty
  q <- G.quantity
  let rend (am, sd, sb) = R.amount gs (Just sd) (Just sb) am
      gen = fmap (\ ((am, _), sb, sd) -> (am, sd, sb)) (G.amount cy q)
  r <- gen
  return $ doParse rend P.amount r

prop_comment =
  renParse R.comment P.comment G.comment

prop_dateTime =
  renParse (fmap Just R.dateTime) P.dateTime G.dateTime

prop_entry = do
  gs <- arbitrary
  cy <- G.genCmdty
  dc <- G.drCr
  qt <- G.quantity
  let rend (iEn, iSd, iSb) = R.entry gs (Just iSd) (Just iSb) iEn
  ((en, _), sb, sd) <- G.entry cy dc qt
  return $ doParse rend P.entry (en, sd, sb)

prop_flag =
  renParse R.flag P.flag G.flag

prop_postingMemoLine = do
  i <- Q.choose (0,4)
  (t, _) <- G.postingMemoLine
  let rend = R.postingMemoLine i
  return $ doParse rend P.postingMemoLine t

prop_postingMemo = do
  b <- arbitrary
  (m, _) <- G.postingMemo
  let rend = R.postingMemo b
  return $ doParse rend P.postingMemo m

prop_transactionMemoLine =
  renParse R.transactionMemoLine P.transactionMemoLine G.transactionMemoLine

prop_transactionMemo =
  renParse R.transactionMemo (fmap snd P.transactionMemo)
           G.transactionMemo

prop_number =
  renParse R.number P.number G.number

prop_quotedLvl1Payee =
  renParse R.quotedLvl1Payee P.quotedLvl1Payee G.quotedLvl1Payee

prop_lvl2Payee =
  renParse R.lvl2Payee P.lvl2Payee G.lvl2Payee

prop_price = do
  gs <- arbitrary
  (pr, _) <- G.price
  let rend = R.price gs
  return $ doParseWithPdct priceEq rend P.price pr

prop_tag =
  renParse R.tag P.tag G.tag

prop_tags =
  renParse R.tags P.tags G.tags

prop_topLineCore =
  renParse R.topLine (fmap toTopLine P.topLine) G.topLineCore

toTopLine :: I.ParsedTopLine -> L.TopLineCore
toTopLine (I.ParsedTopLine dt nu fl pa me _) =
  L.TopLineCore dt nu fl pa (fmap fst me)


prop_transaction = do
  gs <- arbitrary
  let rend = R.transaction gs
      toPair (tl, es) = (toTopLine tl, fmap fst es)
  (genTx, _) <- G.transaction
  return $ doParse rend (fmap toPair P.transaction) genTx

priceEq :: L.PricePoint -> L.PricePoint -> Bool
priceEq (L.PricePoint xdt xpr xsd xsb _)
        (L.PricePoint ydt ypr ysd ysb _)
  = xdt == ydt && xpr == ypr && xsd == ysd && xsb == ysb

runTests :: (Q.Property -> IO Q.Result) -> IO Bool
runTests = $(A.forAllProperties)
