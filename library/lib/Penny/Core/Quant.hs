-- | Components of a non-zero 'Penny.Concrete.T'.

module Penny.Core.Quant where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Gravel as Gravel

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
