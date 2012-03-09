module PennyTest.Copper.Account where

import Control.Applicative ((<$>), (<*>), (<|>), (<*))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import qualified Penny.Copper.Account as A
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.TextNonEmpty as TNE
import PennyTest.Lincoln.Bits ()
import qualified Test.QuickCheck as Q
import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

-- | Render must fail for characters that are not allowed in a level 1
-- account; must succeed for all other characters.
prop_renderSuccess :: B.Account -> Bool
prop_renderSuccess a = expected == actual where
  expected = renderable a
  actual = case A.render a of    
    Ex.Success _ -> True
    Ex.Exception _ -> False

renderable :: B.Account -> Bool
renderable a = if X.any (not . A.lvl1Char) (X.concat $ HT.textList a)
               then False
               else True

newtype RenChar = RenChar { unRenChar :: Char }
                  deriving Show
instance Arbitrary RenChar where
  arbitrary = RenChar <$> Q.suchThat arbitrary A.lvl1Char

renderableChar :: Gen Char
renderableChar = Q.suchThat arbitrary A.lvl1Char

renderableSub :: Gen B.SubAccountName
renderableSub = B.SubAccountName
                <$> (TNE.TextNonEmpty
                     <$> renderableChar
                     <*> (X.pack <$> Q.listOf renderableChar))

renderableAcct :: Gen B.Account
renderableAcct = B.Account
                 <$> (NE.nonEmpty
                      <$> renderableSub
                      <*> Q.listOf renderableSub)

newtype Renderable = Renderable { unRenderable :: B.Account }
                     deriving Show
instance Arbitrary Renderable where
  arbitrary = Renderable <$> renderableAcct

-- | Parsing a rendered render should yield the same thing.
prop_roundTrip :: Parser B.Account -> Renderable -> Bool
prop_roundTrip lvlParser (Renderable r) = let
  rendered = case A.render r of
    Ex.Exception _ -> error "prop_roundTrip: should never happen"
    Ex.Success g -> g
  parser = lvlParser <* P.eof
  parseResult = P.parse parser "" rendered
  in case parseResult of
    Left _ -> False
    Right parsed -> if parsed == r then True else False

-- | Tests the roundtrip when parsing the Level 1 possibility first.
prop_roundTripLvl1First :: Renderable -> Bool
prop_roundTripLvl1First =
  prop_roundTrip (A.lvl1AccountQuoted <|> A.lvl2Account)

-- | Tests the roundtrip when parsing the Level 2 possibility first.
prop_roundTripLvl2First :: Renderable -> Bool
prop_roundTripLvl2First =
  prop_roundTrip (A.lvl2Account <|> A.lvl1AccountQuoted)

-- | All tests from this module
tests :: TF.Test
tests =
  TF.testGroup "Account"
  [ testProperty "Render fails and succeeds as it should"
    prop_renderSuccess

  , testProperty "Parsing a rendered render succeeds, Level 1 first"
    prop_roundTripLvl1First
    
  , testProperty "Parsing a rendered render succeeds, Level 2 first"
    prop_roundTripLvl2First
  ]
