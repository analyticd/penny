-- | Tests of Penny quantities

module PennyTest.Penny.Lincoln.Bits.Qty where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Trans.Class (lift)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Applicative ((<$>), (<*>))
import qualified Test.Framework as TF
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as G
import qualified Test.QuickCheck.Property as P
import qualified Data.Decimal as D
import qualified Penny.Lincoln.Bits.Qty as Q

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Bits.Qty"
  [ testProperty "((x `add` y) `subt` y) == y" prop_addSubt ]

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

-- | Generates 1. The exponent depends on the size parameter (but
-- never exceeds 255.) The mantissa adjusts so the result is 1.
one :: G.Gen D.Decimal
one = G.sized $ \s -> do
  e <- G.choose (0, min (fromIntegral s) 255)
  return $ D.Decimal e (1 * 10 ^ (fromIntegral e))

genQty
  :: (G.Gen D.Decimal)
  -> Ex.ExceptionalT P.Result G.Gen Q.Qty
genQty g = do
  d <- lift g
  case Q.newQty d of
    Nothing -> Ex.throwT (P.failed { P.reason = r })
      where
        r = "Qty generation failed from decimal " ++ show d
    Just q -> return q

-- | > (x + y - y) == x
ex_addSubt :: Ex.ExceptionalT P.Result G.Gen P.Result
ex_addSubt = do
  x <- genQty typicalDecimal
  y <- genQty typicalDecimal
  case (x `Q.add` y) `Q.subt` y of
    Nothing -> return P.failed
    Just r -> return $ P.liftBool (r == x)

-- | > (x + y - y) == x
prop_addSubt :: QC.Gen P.Result
prop_addSubt = Ex.resolveT return ex_addSubt

