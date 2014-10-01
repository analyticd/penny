-- | Components of a non-zero 'Penny.Concrete.T'.

module Penny.Core.Quant where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Core.Polarity as Polarity

-- | Components of a non-zero 'Penny.Qty.T'.
data T a = T
  { novDecs :: NovDecs.T
  , exponent :: Exp.T
  , side :: a
  } deriving (Eq, Ord, Show)

fromGravel :: Gravel.T a -> Maybe (T a)
fromGravel (Gravel.T ex plrty) = case plrty of
  Polarity.Center _ -> Nothing
  Polarity.OffCenter nd sd -> Just $ T nd ex sd

toGravel :: T a -> Gravel.T a
toGravel (T c e s) = Gravel.T e (Polarity.OffCenter c s)
