module PennyTest.Copper.Amount where

import qualified Penny.Copper.Amount as A
import qualified Penny.Lincoln.Bits as B
import qualified PennyTest.Lincoln.Bits as TB
import qualified PennyTest.Lincoln.Meta as TM
import PennyTest.Copper.Commodity (genRCmdty)
import qualified PennyTest.Copper.Qty as TQ

import Control.Applicative ((<$>), (<*), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)

-- | Generates renderable amounts. The commodities are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
genRAmount :: Gen B.Amount
genRAmount = B.Amount <$> TB.genQty <*> genRCmdty

-- | A renderable Amount.  The commodities are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
newtype RAmount = RAmount { unRAmount :: B.Amount }
                  deriving (Show, Eq)

instance Arbitrary RAmount where
  arbitrary = RAmount <$> genRAmount

-- | Parsing a rendered renderable should yield the same amount.
prop_parseAmount ::
  TQ.AnySpecPair
  -> TQ.AnyRadGroup
  -> TM.Format
  -> RAmount
  -> Bool
prop_parseAmount (TQ.AnySpecPair gs) (TQ.AnyRadGroup rg)
  (TM.Format f) (RAmount a) =
  case A.render gs rg f a of
    Nothing -> False
    Just t -> case P.parse (A.amount rg <* P.eof) "" t of
      Left _ -> False
      Right af' -> (a, f) == af'

test_parseAmount :: Test
test_parseAmount = testProperty s prop_parseAmount where
  s = "Parsing a rendered Amount gives the same Amount"

tests :: Test
tests = testGroup "Amount"
        [ test_parseAmount ]
  
