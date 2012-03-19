module PennyTest.Copper.Flag where

import qualified Penny.Copper.Flag as F
import qualified Penny.Lincoln.Bits as B

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Qty ()
import PennyTest.Copper.Util (genTextNonEmpty)

import Control.Applicative ((<$>), (<*))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen)

-- | Generates renderable Flags.
genRFlag :: Gen B.Flag
genRFlag = B.Flag <$> genTextNonEmpty g g where
  g = suchThat arbitrary F.isFlagChar

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
