module PennyTest.Copper.Qty where

import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Bits as B

import qualified PennyTest.Lincoln.Bits as TB

import Control.Applicative ((<*))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)

-- | Parsing a rendered Qty gives the same Qty.
prop_parseQty :: Q.RadGroup -> B.Qty -> Bool
prop_parseQty rg q = let
  rendered = Q.render rg q
  parsed = P.parse (Q.qty rg <* P.eof) "" rendered
  in case parsed of
    Left _ -> False
    Right q' -> q' == q
    
test_parseQty :: 
