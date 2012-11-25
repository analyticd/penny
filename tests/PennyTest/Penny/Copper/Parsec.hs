module PennyTest.Penny.Copper.Parsec where

import Text.Parsec (parse)
import Text.Parsec.Text (Parser)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Property as QP
import qualified PennyTest.Penny.Copper.Gen.Parsers as P
import qualified Penny.Copper.Parsec as C

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
  ]

pTestBy
  :: Show a
  => (a -> a -> Bool)
  -> String
  -> Parser a
  -> Gen (a, Text)
  -> Test
pTestBy eq s p g = testProperty s t
  where
    t = do
      (r, txt) <- g
      case parse p "" txt of
        Left e ->
          let msg = "failed to parse text: " ++ show e
          in return $ QP.failed { QP.reason = msg }
        Right parsed ->
          if eq parsed r
          then return QP.succeeded
          else
            let msg = "result not equal. Parsed result: "
                      ++ show parsed
                      ++ " expected result: " ++ show r
            in return $ QP.failed { QP.reason = msg }

pTest
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> Gen (a, Text)
  -> Test
pTest = pTestBy (==)
