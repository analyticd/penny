module PennyTest.Copper.Number where

import qualified Penny.Copper.Number as N
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

-- | Generate renderable Numbers.
genRNumber :: Gen B.Number
genRNumber = B.Number <$> genTextNonEmpty g g where
  g = suchThat arbitrary N.isNumChar

newtype RNumber = RNumber { unRNumber :: B.Number }
                  deriving (Show, Eq)

instance Arbitrary RNumber where
  arbitrary = RNumber <$> genRNumber

-- | Parsing a rendered Number should give the same thing.
prop_parseRNumber :: RNumber -> Bool
prop_parseRNumber (RNumber n) = case N.render n of
  Nothing -> False
  Just txt -> case P.parse (N.number <* P.eof) "" txt of
    Left _ -> False
    Right n' -> n == n'

test_parseRNumber :: Test
test_parseRNumber = testProperty s prop_parseRNumber where
  s = "Parsing renderable Number should give same Number"

tests :: Test
tests = testGroup "Number"
        [ test_parseRNumber ]
