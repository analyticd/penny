module PennyTest.Copper.Amount where

import qualified Penny.Copper.Amount as A
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import PennyTest.Copper.Qty ()

import Control.Applicative ((<$>), (<*), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)

-- | Generates renderable amounts. The commodities are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
genRAmount :: Gen B.Amount
genRAmount = B.Amount <$> arbitrary <*> genRCmdty

-- | A renderable Amount.  The commodities are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
newtype RAmount = RAmount { unRAmount :: B.Amount }
                  deriving (Show, Eq)

instance Arbitrary RAmount where
  arbitrary = RAmount <$> genRAmount

-- | Parsing a rendered renderable should yield the same amount.
prop_parseAmount ::
  Q.GroupingSpec -- ^ Grouping to left of radix point
  -> Q.GroupingSpec -- ^ Grouping to right of radix point
  -> Q.RadGroup
  -> M.Format
  -> RAmount
  -> Bool
prop_parseAmount gl gr rg f (RAmount a) =
  case A.render gl gr rg f a of
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
  
