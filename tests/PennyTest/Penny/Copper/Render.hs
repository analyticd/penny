-- | Tests renderers by using the generators written for the
-- parser. Generates an item, then renders it, then parses it, and
-- tests to see whether the parsed item is the same as what was
-- generated.
module PennyTest.Penny.Copper.Render where

import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper.Render as R
import qualified PennyTest.Penny.Copper.Gen.Parsers as TP
import Penny.Copper.Parsec as P
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as TF
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec
import qualified Data.Text as X
import qualified Test.QuickCheck.Property as QP
import qualified Test.QuickCheck.Gen as G
import qualified PennyTest.Penny.Copper.Parsec as TC
import Test.QuickCheck (Gen)

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Copper.Render"
  [ pTest "lvl1Acct" P.quotedLvl1Acct
    TP.quotedLvl1Acct R.quotedLvl1Acct

  , pTest "lvl2Acct" P.lvl2Acct TP.lvl2Acct R.lvl2Acct
  , pTest "ledgerAcct" P.ledgerAcct TP.ledgerAcct R.ledgerAcct
  , let g = fmap (\(TP.QuotedLvl1Cmdty c x) -> (c, x))
                 TP.quotedLvl1Cmdty
    in pTest "quotedLvl1Cmdty" P.quotedLvl1Cmdty g R.quotedLvl1Cmdty

  , let g = fmap (\(TP.Lvl2Cmdty c x) -> (c, x)) TP.lvl2Cmdty
    in pTest "lvl2Cmdty" P.lvl2Cmdty g R.lvl2Cmdty

  , let g = fmap (\(TP.Lvl3Cmdty c x) -> (c, x)) TP.lvl3Cmdty
    in pTest "lvl3Cmdty" P.lvl3Cmdty g R.lvl3Cmdty

  , let genRenderer = do
          gs <- lift genGroupSpecs
          return (Just . R.quantity gs)
    in pTestByTG (==) "quantity" P.quantity TP.quantity genRenderer

  , let genRenderer = do
          gs <- lift genGroupSpecs
          return (R.amount gs)
        genAmt = do
          cy <- lift TP.genCmdty
          q <- TP.quantity
          lift $ TP.amount cy q
    in pTestByTG (==) "amount" P.amount genAmt genRenderer

  , pTest "comment" P.comment TP.comment R.comment

  , pTestT "dateTime" P.dateTime TP.dateTime (Just . R.dateTime)

  , let genEn = do
          cy <- lift TP.genCmdty
          dc <- lift TP.drCr
          q <- TP.quantity
          lift $ TP.entry cy dc q
        genRend = do
          gs <- lift genGroupSpecs
          return (\en -> R.entry gs en)
    in pTestByTG (==) "entry" P.entry genEn genRend

  , pTest "flag" P.flag TP.flag R.flag
  , pTest "transactionMemo" ((fmap snd) P.transactionMemo)
    TP.transactionMemo R.transactionMemo

  , pTest "postingMemo" P.postingMemo
    TP.postingMemo R.postingMemo

  , pTest "number" P.number TP.number R.number

  , pTest "quotedLvl1Payee" P.quotedLvl1Payee TP.quotedLvl1Payee
    R.quotedLvl1Payee

  , pTest "lvl2Payee" P.lvl2Payee TP.lvl2Payee R.lvl2Payee

  , pTestByTG TC.samePricePoint "price" P.price TP.price
    (withGroupSpecs R.price)

  , pTest "tag" P.tag TP.tag R.tag
  , pTest "tags" P.tags TP.tags R.tags

  -- Cannot test TopLine and Posting. These functions take a Lincoln
  -- TopLine and a Lincoln Posting. However there is no way to construct
  -- these types from outside of the Lincoln.Transaction module. However
  -- a test of the transaction renderer is sufficient.
  , pTestByTG TC.sameTransaction "transaction" P.transaction
    (TP.maxSize 5 TP.transaction) (withGroupSpecs R.transaction)

  , pTestByTG TC.sameItem "item" P.item
    (TP.maxSize 5 TP.item) (withGroupSpecs R.item)

  , pTestByTG TC.sameLedger "ledger" P.ledger
    (TP.maxSize 5 TP.ledger) (withGroupSpecs R.ledger)
  ]


genGroupSpec :: Gen R.GroupSpec
genGroupSpec = G.elements [ R.NoGrouping, R.GroupLarge, R.GroupAll ]

genGroupSpecs :: Gen R.GroupSpecs
genGroupSpecs = R.GroupSpecs <$> genGroupSpec <*> genGroupSpec

pTestByTG
  :: (Show a, Show b)

  => (a -> b -> Bool)
  -- ^ Tests what is parsed and what is rendered for equality

  -> String
  -- ^ Test description

  -> Parser a
  -- ^ Parses what is rendered

  -> TP.GenT (b, c)
  -- ^ Generates what is rendered. Also generates a rendering,
  -- which is ignored.

  -> TP.GenT (b -> Maybe X.Text)
  -- ^ Rendering function being tested

  -> TF.Test
pTestByTG testEq desc parser gen genRend = testProperty desc prop
  where
    prop = Ex.resolveT return $ do
      (toRender, _) <- gen
      rend <- genRend
      rendered <- case rend toRender of
        Nothing -> Ex.throwT QP.failed { QP.reason = r }
          where r = "could not render text. "
                    ++ "Item that failed to render: " ++ show toRender
        Just r -> return r
      parsed <-
        case Parsec.parse (parser <* Parsec.eof) "" rendered of
          Left e -> Ex.throwT QP.failed { QP.reason = r }
            where r = "could not parse text. "
                      ++ "Item that was rendered: " ++ show toRender
                      ++ " Text that failed to parse: " ++ show rendered
                      ++ " Parse error message: " ++ show e
          Right g -> return g
      if testEq parsed toRender
        then return QP.succeeded
        else let r = "parsed item not equal to original item."
                  ++ " Item that was rendered: " ++ show toRender
                  ++ " Text that was parsed: " ++ show rendered
                  ++ " Parse result: " ++ show parsed
              in return QP.failed { QP.reason = r }


pTestByT
  :: (Show a, Show b)

  => (a -> b -> Bool)
  -- ^ Tests what is parsed and what is rendered for equality

  -> String
  -- ^ Test description

  -> Parser a
  -- ^ Parses what is rendered

  -> TP.GenT (b, c)
  -- ^ Generates what is rendered. Also generates a rendering,
  -- which is ignored.

  -> (b -> Maybe X.Text)
  -- ^ Rendering function being tested

  -> TF.Test
pTestByT testEq desc parser gen rend =
  pTestByTG testEq desc parser gen (return rend)

pTestT
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> TP.GenT (a, b)
  -> (a -> Maybe X.Text)
  -> TF.Test
pTestT = pTestByT (==)

pTestBy
  :: (Show a, Show b)
  => (a -> b -> Bool)
  -> String
  -> Parser a
  -> Gen (b, c)
  -> (b -> Maybe X.Text)
  -> TF.Test
pTestBy testEq desc parser gen rend =
  pTestByT testEq desc parser (lift gen) rend

pTest
  :: (Show a, Eq a)
  => String
  -> Parser a
  -> Gen (a, b)
  -> (a -> Maybe X.Text)
  -> TF.Test
pTest = pTestBy (==)

withGroupSpecs
  :: (R.GroupSpecs -> a -> Maybe X.Text)
  -> TP.GenT (a -> Maybe X.Text)
withGroupSpecs f = lift genGroupSpecs >>= return . f
