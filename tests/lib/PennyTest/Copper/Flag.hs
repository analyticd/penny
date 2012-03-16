module PennyTest.Copper.Flag where

import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.Flag as F
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE
import qualified Penny.Copper.Entry as E
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import PennyTest.Copper.Qty ()
import PennyTest.Copper.Util (wrapTextNonEmptyList)

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Text (pack)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen, listOf)

-- | Generates renderable Flags.
genRFlag :: Gen B.Flag
genRFlag = B.Flag <$> tne where
  tne = TNE.TextNonEmpty
        <$> suchThat arbitrary F.isFlagChar
        <*> (pack <$> listOf (suchThat arbitrary F.isFlagChar))

newtype RFlag = RFlag { unRFlag :: B.Flag }
                deriving (Show, Eq)

instance Arbitrary RFlag where
  arbitrary = RFlag <$> genRFlag

-- | Parsing a rendered renderable should yield the same Flag.
prop_parseRenderable :: RFlag -> Bool
prop_parseRenderable (RFlag fl) = case F.render fl of
  Nothing -> False
  Just txt -> case P.parse (F.flag <* P.eof) "" txt of
    Left _ -> False
    Right fl' -> fl' == fl

test_parseRenderable :: Test
test_parseRenderable = testProperty s prop_parseRenderable where
  s = "Parsing rendered renderable Flag yields same Flag"

tests :: Test
tests = testGroup "Flag"
        [ test_parseRenderable ]
