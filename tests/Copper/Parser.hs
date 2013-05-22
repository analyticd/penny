{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Copper.Parser where

import Control.Applicative ((<*))
import qualified Copper.Gen.Parsers as GP
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.All as A
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck (Gen)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Data.Text as X

parseProp
  :: (Eq b, Show b)
  => P.Parser a
  -> Gen (b, X.Text)
  -> (a -> b)
  -> Q.Property
parseProp p g f = Q.forAll g $ \(b, x) ->
  case P.parse (p <* P.eof) "" x of
    Left e -> QCP.failed { QCP.reason = "parse failed: " ++ show e }
    Right gd ->
      if f gd == b
      then QCP.succeeded
      else QCP.failed { QCP.reason = "result not identical to expected: "
                                     ++ show (f gd) }

prop_lvl1SubAcct = parseProp CP.lvl1SubAcct GP.lvl1SubAcct id
prop_lvl1FirstSubAcct =
  parseProp CP.lvl1FirstSubAcct GP.lvl1FirstSubAcct id

prop_lvl1OtherSubAcct =
  parseProp CP.lvl1OtherSubAcct GP.lvl1OtherSubAcct id

prop_lvl1Acct =
  parseProp CP.lvl1Acct GP.lvl1Acct id

prop_quotedLvl1Acct = parseProp CP.quotedLvl1Acct GP.quotedLvl1Acct id

prop_lvl2FirstSubAcct =
  parseProp CP.lvl2FirstSubAcct GP.lvl2FirstSubAcct id

prop_lvl2OtherSubAcct =
  parseProp CP.lvl2OtherSubAcct GP.lvl2OtherSubAcct id

prop_lvl2Acct =
  parseProp CP.lvl2Acct GP.lvl2Acct id

prop_ledgerAcct =
  parseProp CP.ledgerAcct GP.ledgerAcct id

prop_lvl1Cmdty =
  parseProp CP.lvl1Cmdty GP.lvl1Cmdty id

prop_quotedLvl1Cmdty =
  let gen = fmap (\(GP.QuotedLvl1Cmdty c x) -> (c, x)) GP.quotedLvl1Cmdty
  in parseProp CP.quotedLvl1Cmdty gen id

prop_lvl2Cmdty =
  let gen = fmap (\(GP.Lvl2Cmdty c x) -> (c, x)) GP.lvl2Cmdty
  in parseProp CP.lvl2Cmdty gen id


prop_lvl3Cmdty =
  let gen = fmap (\(GP.Lvl3Cmdty c x) -> (c, x)) GP.lvl3Cmdty
  in parseProp CP.lvl3Cmdty gen id


prop_quantity =
  parseProp CP.quantity GP.quantity id

doParse
  :: (Show a, Eq a)
  => P.Parser a
  -> a
  -> X.Text
  -> QCP.Result
doParse p a x = doParse' p (show a) (== a) x

doParse'
  :: Show a
  => P.Parser a
  -> String
  -> (a -> Bool)
  -> X.Text
  -> QCP.Result
doParse' p b pd x = case P.parse (p <* P.eof) "" x of
  Left e -> QCP.failed
    { QCP.reason = "parse failed: " ++ show e }
  Right g -> if pd g then QCP.succeeded
             else QCP.failed { QCP.reason = "items do not match."
                               ++ "parsed item: " ++ show g
                               ++ "expected item: " ++ b }

prop_leftCmdtyLvl1Amt = do
  cy <- GP.quotedLvl1Cmdty
  q <- GP.quantity
  ((a, x), sb) <- GP.leftCmdtyLvl1Amt cy q
  return $ doParse CP.leftCmdtyLvl1Amt (a, L.CommodityOnLeft, sb) x

prop_leftCmdtyLvl3Amt = do
  cy <- GP.lvl3Cmdty
  q <- GP.quantity
  ((a, x), sb) <- GP.leftCmdtyLvl3Amt cy q
  return $ doParse CP.leftCmdtyLvl3Amt (a, L.CommodityOnLeft, sb) x

prop_leftSideCmdtyAmt = do
  cy <- Q.oneof [ fmap Left GP.quotedLvl1Cmdty
                , fmap Right GP.lvl3Cmdty ]
  q <- GP.quantity
  ((a, x), sb) <- GP.leftSideCmdtyAmt cy q
  return $ doParse CP.leftSideCmdtyAmt (a, L.CommodityOnLeft, sb) x

prop_rightSideCmdtyAmt = do
  cy <- Q.oneof [ fmap Left GP.quotedLvl1Cmdty
                , fmap Right GP.lvl2Cmdty ]
  q <- GP.quantity
  ((a, x), sb) <- GP.rightSideCmdtyAmt cy q
  return $ doParse CP.rightSideCmdtyAmt (a, L.CommodityOnRight, sb) x

prop_amount = do
  cy <- GP.genCmdty
  q <- GP.quantity
  ((a, x), sb, sd) <- GP.amount cy q
  return $ doParse CP.amount (a, sd, sb) x

prop_comment =
  parseProp CP.comment GP.comment id

prop_year =
  parseProp CP.year GP.year id

prop_month =
  parseProp CP.month GP.month id

prop_day =
  parseProp CP.day GP.day id

prop_date =
  parseProp CP.date GP.date id

prop_hours =
  parseProp CP.hours GP.hours id

prop_minutes =
  parseProp CP.minutes GP.minutes id

prop_seconds =
  parseProp CP.seconds GP.seconds id

prop_time =
  parseProp CP.time GP.time id

prop_tzNumber =
  parseProp CP.tzNumber GP.tzNumber id

prop_timeZone =
  parseProp CP.timeZone GP.timeZone id

prop_timeWithZone =
  parseProp CP.timeWithZone GP.timeWithZone id

prop_dateTime =
  parseProp CP.dateTime GP.dateTime id

prop_debit =
  parseProp CP.debit GP.debit id

prop_credit =
  parseProp CP.credit GP.credit id

prop_drCr =
  parseProp CP.drCr GP.drCr id

prop_entry = do
  cy <- GP.genCmdty
  dc <- GP.drCr
  q <- GP.quantity
  ((en, x), sb, sd) <- GP.entry cy dc q
  return $ doParse CP.entry (en, sd, sb) x

prop_flag =
  parseProp CP.flag GP.flag id

prop_postingMemoLine =
  parseProp CP.postingMemoLine GP.postingMemoLine id

prop_postingMemo =
  parseProp CP.postingMemo GP.postingMemo id

prop_transactionMemoLine =
  parseProp CP.transactionMemoLine GP.transactionMemoLine id

prop_transactionMemo =
  parseProp CP.transactionMemo GP.transactionMemo snd

prop_number =
  parseProp CP.number GP.number id

prop_lvl1Payee =
  parseProp CP.lvl1Payee GP.lvl1Payee id

prop_quotedLvl1Payee =
  parseProp CP.quotedLvl1Payee GP.quotedLvl1Payee id

prop_lvl2Payee =
  parseProp CP.lvl2Payee GP.lvl2Payee id

prop_fromCmdty = do
  cy <- Q.oneof [ fmap Left GP.quotedLvl1Cmdty
                , fmap Right GP.lvl2Cmdty ]
  let (fr, x) = GP.fromCmdty cy
  return $ doParse CP.fromCmdty fr x

prop_price = Q.forAll GP.price $ \(pp, x) ->
  let pd pp' = L.dateTime pp == L.dateTime pp'
               && L.price pp == L.price pp'
               && L.ppSide pp == L.ppSide pp'
               && L.ppSpaceBetween pp == L.ppSpaceBetween pp'
  in doParse' CP.price (show pp) pd x

prop_tag =
  parseProp CP.tag GP.tag id

prop_tags =
  parseProp CP.tags GP.tags id

prop_topLinePayee =
  parseProp CP.topLinePayee GP.topLinePayee id

prop_topLineFlagNum =
  parseProp CP.topLineFlagNum GP.topLineFlagNum id

parsedTopLineToCore :: C.ParsedTopLine -> L.TopLineCore
parsedTopLineToCore (C.ParsedTopLine dt nu fl pa me _) =
  L.TopLineCore dt nu fl pa (fmap fst me)

prop_topLineCore = parseProp CP.topLine GP.topLineCore
                             parsedTopLineToCore

prop_flagNumPayee =
  parseProp CP.flagNumPayee GP.flagNumPayee id

prop_postingAcct =
  parseProp CP.postingAcct GP.postingAcct id

prop_posting = do
  doJust <- Q.frequency [(3, return True), (1, return False)]
  ((pc, x), mayEn) <- if doJust
    then do
      cy <- GP.genCmdty
      dc <- GP.drCr
      q <- GP.quantity
      en <- GP.entry cy dc q
      GP.posting (Just en)
    else GP.posting Nothing
  let pd (pc', _, mayEn') = pc' == pc && mayEn' == mayEn
  return $ doParse' CP.posting (show (pc, mayEn)) pd x

prop_transaction = do
  ((tlc, es), x) <- GP.transaction
  let pd (ptl, es') = (parsedTopLineToCore ptl, fmap fst es') == (tlc, es)
  return $ doParse' CP.transaction (show (tlc, es)) pd x

runTests :: (Q.Property -> IO Q.Result) -> IO Bool
runTests = $(A.forAllProperties)
