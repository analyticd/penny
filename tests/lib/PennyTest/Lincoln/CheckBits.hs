-- | Ensure that the Bits are behaving as they should.

module PennyTest.Lincoln.CheckBits where

import Control.Applicative ((<$>))
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as P
import qualified PennyTest.Lincoln.Generators as G
import qualified Penny.Lincoln as L
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as T

-- | MinsToOffset succeeds when offset is between -840 and 840.
goodOffsets :: T.Test
goodOffsets = testProperty n t
  where
    n = "MinsToOffset succeeds when offset is between -840 and 840"
    t = do
      i <- Q.choose (-840, 840)
      case L.minsToOffset i of
        Just _ -> return P.succeeded
        _ -> return $
             P.failed { P.reason = "failed with offset: " ++ show i }

-- | MinsToOffset fails when offset is not between 840 and 840.
badOffsets :: T.Test
badOffsets = testProperty n t
  where
    n = "MinsToOffset fails when offset is not between 840 and 840"
    t = do
      i <- Q.oneof [Q.choose (minBound, -841),
                    Q.choose (841, maxBound)]
      case L.minsToOffset i of
        Nothing -> return P.succeeded
        _ -> return $
             P.failed { P.reason = "failed with offset: " ++ show i }

-- | newPrice succeeds if the commodities are different.
newPriceSuccess :: T.Test
newPriceSuccess = testProperty n t
  where
    n = "newPrice succeeds if commodities are different"
    t = do
      fr <- G.from
      to <- Q.suchThat G.to (\(L.To cTo) -> cTo /= (L.unFrom fr))
      d <- G.randomDecimal
      case L.CountPerUnit <$> (L.newQty d) of
        Nothing ->
          return $ P.failed { P.reason = "could not generate cost per unit "
                                         ++ "with decimal: " ++ show d }
        Just cpu ->
          case L.newPrice fr to cpu of
            Nothing ->
              return $ P.failed { P.reason = "price generation failed with "
                                             ++ "from commodity: " ++ show fr
                                             ++ " to commodity: " ++ show to }
            Just _ -> return P.succeeded

        
-- | newPrice fails if the commodities are the same.
newPriceFail :: T.Test
newPriceFail = testProperty n t
  where
    n = "newPrice succeeds if commodities are different"
    t = do
      fr <- G.from
      let to = L.To . L.unFrom $ fr
      d <- G.randomDecimal
      case L.CountPerUnit <$> (L.newQty d) of
        Nothing ->
          return $ P.failed { P.reason = "could not generate cost per unit "
                                         ++ "with decimal: " ++ show d }
        Just cpu ->
          case L.newPrice fr to cpu of
            Just _ ->
              return $ P.failed { P.reason = "price generation succeeded with "
                                             ++ "from commodity: " ++ show fr
                                             ++ " to commodity: " ++ show to }
            Nothing -> return P.succeeded

        
