module PennyTest.Penny.Copper.Account where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck.Gen as G
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Gen (Gen)
import qualified PennyTest.Penny.Copper.Util as TU
import qualified Penny.Copper.Account as A
import qualified Penny.Lincoln as L
import qualified Text.Parsec as Ps

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Copper.Account"
  [ testProperty
    "Parsing rendered Level 1 account should yield same thing"
    test_parseAcctRenderLvl1

  , testProperty
    "Parsing rendered Level 2 account should yield same thing"
    test_parseAcctRenderLvl2
  ]

lvl1AcctChar :: Gen Char
lvl1AcctChar = G.suchThat (G.oneof [TU.unicodeAll, TU.asciiAll]) A.lvl1Char

-- | Generates Level 1 sub-accounts; however, does not make sure they
-- are valid.
lvl1SubAcct :: Gen L.SubAccount
lvl1SubAcct = (L.SubAccount . X.pack) <$> G.listOf1 lvl1AcctChar

-- | Generates Level 1 accounts; however, does not make sure they are
-- valid.
lvl1Account :: Gen L.Account
lvl1Account = L.Account <$> G.listOf1 lvl1SubAcct

lvl2AcctFirstChar :: Gen Char
lvl2AcctFirstChar = G.oneof [TU.unicodeAll, TU.letter]

lvl2AcctRemainingChar :: Gen Char
lvl2AcctRemainingChar = G.suchThat (G.oneof [TU.unicodeAll, TU.asciiAll])
                        A.lvl2RemainingChar

-- | Generates the first sub-account of a Level 2 account; however,
-- does not make sure it is valid.
lvl2SubAccountFirst :: Gen L.SubAccount
lvl2SubAccountFirst =
  f <$> lvl2AcctFirstChar <*> G.listOf lvl2AcctRemainingChar
  where
    f c cs = L.SubAccount (X.pack (c:cs))

-- | Generates the remaining sub-accounts of a Level 2 account;
-- however, does not make sure it is valid.
lvl2SubAccountRest :: Gen L.SubAccount
lvl2SubAccountRest = (L.SubAccount . X.pack)
                     <$> G.listOf1 lvl2AcctRemainingChar

-- | Generates Level 2 accounts; however, does not make sure they are valid.
lvl2Account :: Gen L.Account
lvl2Account = f <$> lvl2SubAccountFirst <*> G.listOf lvl2SubAccountRest
  where
    f a1 ar = L.Account (a1:ar)

-- | Generates an account and renders it; returns the account
-- and the rendering. Fails if the render fails.
renderLevel
  :: Gen L.Account
  -- ^ Use this generator
  -> String
  -- ^ Level number (1 or 2)
  -> Ex.ExceptionalT P.Result Gen (L.Account, X.Text)
renderLevel g s = do
  a <- lift g
  case A.render a of
    Nothing -> Ex.throwT (P.failed { P.reason = msg })
      where
        msg = "failed to render a created Level " ++ s ++ " account"
    Just x -> return (a, x)

-- | Using the supplied generator, parser, and level description,
-- generate an account, parse it, and succeeds if parsing the render
-- gives the same result.
parseRender
  :: Gen L.Account
  -> String
  -- ^ Level number (1 or 2)
  -> Gen P.Result
parseRender g s = Ex.resolveT return $ do
  (a, x) <- renderLevel g s
  let parser = A.lvl1AccountQuoted <|> A.lvl2Account
  a' <- case Ps.parse parser "" x of
    Left e -> Ex.throwT $ P.failed { P.reason = msg }
      where
        msg = "rendered text did not parse. Rendered text: "
              ++ show x
              ++ " error message: " ++ show e
    Right txt -> return txt
  return $ if a' == a
    then P.succeeded
    else P.failed
      { P.reason = "parsed account did not match rendered account" }

-- | Parsing a rendered level 1 account should yield same thing
test_parseAcctRenderLvl1 :: Gen P.Result
test_parseAcctRenderLvl1 = parseRender lvl1Account "1"

-- | Parsing a rendered level 2 account should yield same thing
test_parseAcctRenderLvl2 :: Gen P.Result
test_parseAcctRenderLvl2 = parseRender lvl2Account "2"


