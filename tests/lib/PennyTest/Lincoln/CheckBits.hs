-- | Ensure that the Bits are behaving as they should.

module PennyTest.Lincoln.CheckBits where

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

-- | Convert succeeds when it should.
