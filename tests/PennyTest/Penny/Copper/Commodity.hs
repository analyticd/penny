module PennyTest.Penny.Copper.Commodity where

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
import qualified Penny.Copper.Commodity as CC
import qualified Penny.Lincoln as L
import qualified Text.Parsec as Ps
import Text.Parsec.Text (Parser)

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Copper.Commodity"
  [ testProperty
    "Parsing rendered quoted level 1 should give same thing"
    test_parseQuotedLvl1

  , testProperty
    "Parsing rendered level 2 should give same thing"
    test_parseLvl2

  , testProperty
    "Parsing rendered level 3 should give same thing"
    test_parseLvl3

  , testProperty
    "Parsing rendered left-side commodity should give same thing"
    test_parseLeftSide

  , testProperty
    "Parsing rendered right-side commodity should give same thing"
    test_parseRightSide
  ]

lvl1Char :: Gen Char
lvl1Char = G.suchThat (G.oneof [TU.unicodeAll, TU.asciiAll])
           (/= '"')

lvl1Cmdty :: Gen L.Commodity
lvl1Cmdty = (L.Commodity . X.pack) <$> G.listOf1 lvl1Char

lvl2FirstChar :: Gen Char
lvl2FirstChar = G.oneof [TU.unicodeAll, TU.letter, return '$']

lvl2OtherChars :: Gen Char
lvl2OtherChars = G.suchThat (G.oneof [TU.asciiAll, TU.unicodeAll])
                 (/= ' ')

lvl2Cmdty :: Gen L.Commodity
lvl2Cmdty = f <$> lvl2FirstChar <*> G.listOf lvl2OtherChars
  where
    f c cs = L.Commodity (X.pack (c:cs))

lvl3Char :: Gen Char
lvl3Char = G.oneof [TU.unicodeAll, TU.letter, return '$']

lvl3Cmdty :: Gen L.Commodity
lvl3Cmdty = (L.Commodity . X.pack) <$> G.listOf1 lvl3Char

-- | Using the given generator and renderer, generate a commodity and
-- render it. Returns the commodity and the rendered text. Fails if
-- the render fails.
genRender
  :: Gen L.Commodity
  -> (L.Commodity -> Maybe X.Text)
  -> Ex.ExceptionalT P.Result Gen (L.Commodity, X.Text)
genRender g r = do
  c <- lift g
  case r c of
    Nothing -> Ex.throwT P.failed { P.reason = msg }
      where msg = "could not render generated commodity. "
                  ++ "Generated commodity: " ++ show c
    Just x -> return (c, x)

-- | Using the given generator, renderer, and parser, generate a
-- commodity, render it, and parse it. Fails if the parse does not
-- yield the same thing as the generated commodity.
genRenderParse
  :: Gen L.Commodity
  -> (L.Commodity -> Maybe X.Text)
  -> Parser L.Commodity
  -> Gen P.Result
genRenderParse g r p = Ex.resolveT return $ do
  (c, t) <- genRender g r
  let parsed = Ps.parse p "" t
  c' <- case parsed of
    Left _ -> Ex.throwT P.failed { P.reason = msg }
      where msg = "could not parse commodity"
    Right result -> return result
  return $ if c' == c
    then P.succeeded
    else let msg = "parsed commodity is not equal "
                   ++ "to original commodity"
          in P.failed { P.reason = msg }


-- | Parsing rendered quoted level 1 should give same thing
test_parseQuotedLvl1 :: Gen P.Result
test_parseQuotedLvl1 =
  genRenderParse lvl1Cmdty CC.renderQuotedLvl1
    CC.quotedLvl1Cmdty

-- | Parsing rendered level 2 should give same thing
test_parseLvl2 :: Gen P.Result
test_parseLvl2 =
  genRenderParse lvl2Cmdty CC.renderLvl2 CC.lvl2Cmdty

-- | Parsing rendered level 3 should give same thing
test_parseLvl3 :: Gen P.Result
test_parseLvl3 =
  genRenderParse lvl3Cmdty CC.renderLvl3 CC.lvl3Cmdty

-- | Parsing rendered left-side commodity should give same thing
test_parseLeftSide :: Gen P.Result
test_parseLeftSide = genRenderParse g r CC.leftSideCmdty
  where
    g = G.oneof [lvl1Cmdty, lvl3Cmdty]
    r c = CC.renderLvl3 c <|> CC.renderQuotedLvl1 c

-- | Parsing rendered right-side commodity should give same thing
test_parseRightSide :: Gen P.Result
test_parseRightSide = genRenderParse g r CC.rightSideCmdty
  where
    g = G.oneof [lvl1Cmdty, lvl2Cmdty]
    r c = CC.renderLvl2 c <|> CC.renderQuotedLvl1 c
