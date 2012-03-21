module PennyTest.Copper.Qty where

import qualified Penny.Copper.Qty as Q
import qualified PennyTest.Lincoln.Bits as TB

import Control.Applicative ((<*), (<$>), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, elements,
                        Property, property, Gen)

genRadGroup :: Gen Q.RadGroup
genRadGroup = elements [
    Q.periodComma, Q.periodSpace, Q.commaPeriod, Q.commaSpace ]

newtype AnyRadGroup = AnyRadGroup Q.RadGroup
                      deriving (Show, Eq)
instance Arbitrary AnyRadGroup where
  arbitrary = AnyRadGroup <$> genRadGroup

genGroupingSpec :: Gen Q.GroupingSpec
genGroupingSpec = elements [ Q.NoGrouping, Q.GroupLarge, Q.GroupAll ]

newtype AnyGroupingSpec = AnyGroupingSpec Q.GroupingSpec
                          deriving (Eq, Show)
instance Arbitrary AnyGroupingSpec where
  arbitrary = AnyGroupingSpec <$> genGroupingSpec

newtype AnySpecPair = AnySpecPair (Q.GroupingSpec, Q.GroupingSpec)
                      deriving (Eq, Show)
instance Arbitrary AnySpecPair where
  arbitrary = AnySpecPair <$>
              ((,) <$> genGroupingSpec <*> genGroupingSpec)

-- | Parsing a rendered, quoted Qty gives the same Qty.
prop_parseQty ::
  AnyRadGroup
  -> (AnyGroupingSpec, AnyGroupingSpec)
  -> Property
prop_parseQty (AnyRadGroup rg) gs = do
  let (AnyGroupingSpec gl, AnyGroupingSpec gr) = gs
  q <- TB.genQty
  let rendered = Q.quote . Q.renderUnquoted rg (gl, gr) $ q
      parsed = P.parse (Q.qty rg <* P.eof) "" rendered
  case parsed of
    Left _ -> property False
    Right q' -> property $ q' == q
    
test_parseQty :: Test
test_parseQty = testProperty s prop_parseQty where
  s = "Parsing a rendered Qty gives same Qty"

tests :: Test
tests = testGroup "Qty"
        [ test_parseQty ]


