-- | Tests of Penny quantities

module PennyTest.Penny.Lincoln.Bits.Qty where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Applicative ((<$>), (<*>))
import qualified Test.Framework as TF
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as G
import qualified Data.Decimal as D

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Bits.Qty"
  []

-- | Generates very small positive Decimals.
verySmallDecimal :: G.Gen D.Decimal
verySmallDecimal =
  D.Decimal
  <$> G.choose (10, 255)
  <*> G.choose (1, 1 * 10 ^ (6 :: Int))

-- | Generates typical decimals in a financial setting. Have exponents
-- between 0 and 5, and mantissas that are between 1 and 2 raised to
-- the power of the size parameter.
typicalDecimal :: G.Gen D.Decimal
typicalDecimal = G.sized $ \s ->
  D.Decimal
  <$> G.choose (0, 5)
  <*> G.choose (1, 2 ^ s)

