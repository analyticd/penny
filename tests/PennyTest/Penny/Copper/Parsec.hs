module PennyTest.Penny.Copper.Parsec where

import Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Trans.Class (lift)
import Text.Parsec (parse)
import Text.Parsec.Text (Parser)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as QP
import qualified PennyTest.Penny.Copper.Gen.Parsers as P
import qualified Penny.Copper.Parsec as C
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction.Unverified as U

import Data.Text (Text)

tests :: Test
tests = testGroup "PennyTest.Penny.Copper.Parsec"
  [ pTest "lvl1SubAcct"
    C.lvl1SubAcct P.lvl1SubAcct

  , pTest "lvl1FirstSubAcct"
    C.lvl1FirstSubAcct P.lvl1FirstSubAcct

  , pTest "lvl1OtherSubAcct"
    C.lvl1OtherSubAcct P.lvl1OtherSubAcct

  , pTest "lvl1Acct"
    C.lvl1Acct P.lvl1Acct

  , pTest "quotedLvl1Acct"
    C.quotedLvl1Acct P.quotedLvl1Acct

  , pTest "lvl2FirstSubAcct"
    C.lvl2FirstSubAcct P.lvl2FirstSubAcct

  , pTest "lvl2OtherSubAcct"
    C.lvl2OtherSubAcct P.lvl2OtherSubAcct

  , pTest "lvl2Acct"
    C.lvl2Acct P.lvl2Acct

  , pTest "ledgerAcct"
    C.ledgerAcct P.ledgerAcct

  , pTest "lvl1Cmdty"
    C.lvl1Cmdty P.lvl1Cmdty

  , pTest "quotedLvl1Cmdty"
    C.quotedLvl1Cmdty
    (fmap (\(P.QuotedLvl1Cmdty c x) -> (c, x)) P.quotedLvl1Cmdty)

  , pTest "lvl2Cmdty"
    C.lvl2Cmdty
    (fmap (\(P.Lvl2Cmdty c x) -> (c, x)) P.lvl2Cmdty)

  , pTest "lvl3Cmdty"
    C.lvl3Cmdty
    (fmap (\(P.Lvl3Cmdty c x) -> (c, x)) P.lvl3Cmdty)

  , pTestT "quantity"
    C.quantity P.quantity

  , let g = do
          c <- lift P.quotedLvl1Cmdty
          q <- P.quantity
          lift $ P.leftCmdtyLvl1Amt c q
    in pTestT "leftCmdtyLvl1Amt"
       C.leftCmdtyLvl1Amt g

  , let g = do
          c <- lift P.lvl3Cmdty
          q <- P.quantity
          lift $ P.leftCmdtyLvl3Amt c q
    in pTestT "leftCmdtyLvl3Amt"
       C.leftCmdtyLvl3Amt g

  , let g = do
          c <- lift $ Q.oneof [ fmap Left P.quotedLvl1Cmdty
                              , fmap Right P.lvl3Cmdty ]
          q <- P.quantity
          lift $ P.leftSideCmdtyAmt c q
    in pTestT "leftSideCmdtyAmt"
       C.leftSideCmdtyAmt g

  , let g = do
          wc <- P.rightSideCmdty
          let (c, x) = case wc of
                Left (P.QuotedLvl1Cmdty ic ix) -> (ic, ix)
                Right (P.Lvl2Cmdty ic ix) -> (ic, ix)
          return (c, x)
    in pTest "rightSideCmdty" C.rightSideCmdty g

  , let g = do
          c <- lift P.rightSideCmdty
          q <- P.quantity
          lift $ P.rightSideCmdtyAmt c q
    in pTestT "rightSideCmdtyAmt"
       C.rightSideCmdtyAmt g

  , let g = do
          c <- lift P.genCmdty
          q <- P.quantity
          lift $ P.amount c q
    in pTestT "amount" C.amount g

  , pTest "comment" C.comment P.comment
  , pTest "year" C.year P.year
  , pTest "month" C.month P.month
  , pTest "day" C.day P.day
  , pTestT "date" C.date P.date
  , pTestT "hours" C.hours P.hours
  , pTestT "minutes" C.minutes P.minutes
  , pTestT "seconds" C.seconds P.seconds
  , pTestT "time" C.time P.time

  , testProperty "tzSign" $ do
      i <- Q.arbitrary
      (fExp, txt) <- P.tzSign
      let eiFParsed = parse C.tzSign "" txt
      case eiFParsed of
        Left e ->
          let msg = "failed to parse time zone sign: " ++ show e
          in return $ QP.failed { QP.reason = msg }
        Right fParsed -> return $
          if fParsed i == fExp i
          then QP.succeeded
          else let msg = "sign functions do not match"
               in QP.failed { QP.reason = msg }

  , pTest "tzNumber" C.tzNumber P.tzNumber
  , pTestT "timeZone" C.timeZone P.timeZone
  , pTestT "timeWithZone" C.timeWithZone P.timeWithZone
  , pTestT "dateTime" C.dateTime P.dateTime
  , pTest "debit" C.debit P.debit
  , pTest "credit" C.credit P.credit

  , let g = do
          c <- lift P.genCmdty
          dc <- lift P.drCr
          q <- P.quantity
          lift $ P.entry c dc q
    in pTestT "entry" C.entry g

  , pTest "flag" C.flag P.flag
  , pTest "postingMemoLine" C.postingMemoLine P.postingMemoLine
  , pTest "postingMemo" C.postingMemo P.postingMemo
  , pTest "transactionMemoLine" C.transactionMemoLine P.transactionMemoLine

  , let p (_, pm) em = pm == em
    in pTestBy p "transactionMemo" C.transactionMemo P.transactionMemo

  , pTest "number" C.number P.number
  , pTest "payee" C.lvl1Payee P.lvl1Payee
  , pTest "quotedLvl1Payee" C.quotedLvl1Payee P.quotedLvl1Payee
  , pTest "lvl2Payee" C.lvl2Payee P.lvl2Payee

  , let g = do
          c <- Q.oneof [ fmap Left P.quotedLvl1Cmdty
                       , fmap Right P.lvl2Cmdty ]
          return $ P.fromCmdty c
    in pTest "fromCmdty" C.fromCmdty g

  , pTestByT samePricePoint "price"
    C.price P.price

  , pTest "tag" C.tag P.tag
  , pTest "tags" C.tags P.tags
  , pTest "topLinePayee" C.topLinePayee P.topLinePayee
  , pTest "topLineFlagNum" C.topLineFlagNum P.topLineFlagNum
  , pTestByT sameTopLine "topLine" C.topLine P.topLine

  , pTest "flagNumPayee" C.flagNumPayee
    P.flagNumPayee

  , pTest "postingAcct" C.postingAcct P.postingAcct

  , let g = do
          c <- lift P.genCmdty
          dc <- lift P.drCr
          q <- P.quantity
          e <- lift $ Q.frequency [ (3, fmap Just (P.entry c dc q))
                                  , (1, return Nothing) ]
          lift $ P.posting e
    in pTestByT samePosting "posting" C.posting g

  ]


pTestByT
  :: (Show a, Show b)
  => (a -> b -> Bool)
  -> String
  -> Parser a
  -> P.GenT (b, Text)
  -> Test
pTestByT eq s p g = testProperty s t
  where
    t = Ex.resolveT return $ do
      (r, txt) <- g
      case parse p "" txt of
        Left e ->
          let msg = "failed to parse text: " ++ show e
                    ++ " expected result: " ++ show r
                    ++ " string being parsed: " ++ show txt
          in return $ QP.failed { QP.reason = msg }
        Right parsed ->
          if eq parsed r
          then return QP.succeeded
          else
            let msg = "result not equal. Parsed result: "
                      ++ show parsed
                      ++ " expected result: " ++ show r
                      ++ " string being parsed: " ++ show txt
            in return $ QP.failed { QP.reason = msg }

pTestT
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> P.GenT (a, Text)
  -> Test
pTestT = pTestByT (==)


pTestBy
  :: (Show a, Show b)
  => (a -> b -> Bool)
  -> String
  -> Parser a
  -> Gen (b, Text)
  -> Test
pTestBy eq s p g = pTestByT eq s p (lift g)

pTest
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> Gen (a, Text)
  -> Test
pTest = pTestBy (==)

-- | Are these the same price? Excludes metadata such as line numbers,
-- but does examine format.
samePricePoint :: L.PricePoint -> L.PricePoint -> Bool
samePricePoint pp1 pp2 =
  (L.dateTime pp1 == L.dateTime pp2)
  && (L.price pp1 == L.price pp2)
  && ((L.priceFormat . L.ppMeta $ pp1)
        == (L.priceFormat . L.ppMeta $ pp2))

-- | Does not compare TopLineMeta.
sameTopLine :: U.TopLine -> U.TopLine -> Bool
sameTopLine x y =
  (U.tDateTime x == U.tDateTime y)
  && (U.tFlag x == U.tFlag y)
  && (U.tNumber x == U.tNumber y)
  && (U.tPayee x == U.tPayee y)
  && (U.tMemo x == U.tMemo y)

-- | Compares Posting Format only.
samePosting :: U.Posting -> U.Posting -> Bool
samePosting x y =
  (U.pPayee x == U.pPayee y)
  && (U.pNumber x == U.pNumber y)
  && (U.pFlag x == U.pFlag y)
  && (U.pAccount x == U.pAccount y)
  && (U.pTags x == U.pTags y)
  && (U.pEntry x == U.pEntry y)
  && (U.pMemo x == U.pMemo y)
  && ((L.postingFormat . U.pMeta $ x)
      == (L.postingFormat . U.pMeta $ y))
