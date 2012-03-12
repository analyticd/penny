module PennyTest.Copper.Qty where

import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Bits as B

import qualified PennyTest.Lincoln.Bits as TB

import Control.Applicative ((<*))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, Gen, elements)

instance Arbitrary Q.RadGroup where
  arbitrary = elements [
    Q.periodComma, Q.periodSpace, Q.commaPeriod, Q.commaSpace ]

instance Arbitrary Q.GroupingSpec where
  arbitrary = elements [ Q.NoGrouping, Q.GroupLarge, Q.GroupAll ]

-- | Parsing a rendered, quoted Qty gives the same Qty.
prop_parseQty ::
  Q.RadGroup
  -> Q.GroupingSpec
  -> Q.GroupingSpec
  -> B.Qty
  -> Bool
prop_parseQty rg gw gd q = let
  rendered = Q.quote . Q.renderUnquoted rg gw gd $ q
  parsed = P.parse (Q.qty rg <* P.eof) "" rendered
  in case parsed of
    Left _ -> False
    Right q' -> q' == q
    
test_parseQty :: Test
test_parseQty = testProperty s prop_parseQty where
  s = "Parsing a rendered Qty gives same Qty"

tests :: Test
tests = testGroup "Qty"
        [ test_parseQty ]


