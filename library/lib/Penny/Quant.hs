-- | Components of a non-zero 'Penny.Concrete.T'.

module Penny.Quant where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Exp as Exp
import qualified Penny.Lincoln.Gravel as Gravel

-- | Components of a non-zero 'Penny.Qty.T'.
data T a = T
  { novDecs :: NovDecs.T
  , exponent :: Exp.T
  , side :: a
  } deriving (Eq, Ord, Show)

fromGravel :: Gravel.T a -> Maybe (T a)
fromGravel (Gravel.T mayC e) = case mayC of
  Nothing -> Nothing
  Just (s, nd) -> Just $ T nd e s

toGravel :: T a -> Gravel.T a
toGravel (T c e s) = Gravel.T (Just (s, c)) e
