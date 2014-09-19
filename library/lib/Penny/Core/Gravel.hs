module Penny.Core.Gravel where

import qualified Penny.Core.Anna as NovDecs
import qualified Penny.Core.Exp as Exponent
import qualified Penny.Core.Cement as Cement
import qualified Penny.Core.Coeff as Coefficient
import Deka.Dec
import qualified Penny.Core.NovSign as NovSign

-- | Components of a sided concrete type, such as 'Penny.Qty.T'.  The
-- type might be zero, in which case it has no coefficient and no
-- side; otherwise, it has both a coefficient and a side.  Whether it
-- is zero or not, it always has an exponent (this differentiates
-- between, for example, @0@ and @0.000@.)
data T a = T
  { coefficient :: Maybe (a, NovDecs.T)
  , exponent :: Exponent.T
  } deriving (Eq, Ord, Show)

toCement :: (a -> Sign) -> T a -> Cement.T
toCement toSign (T mayC e) = Cement.T c e
  where
    c = case mayC of
      Nothing -> Coefficient.Zero
      Just (s, nd) -> Coefficient.NonZero $ NovSign.T nd (toSign s)

fromCement :: (Sign -> a) -> Cement.T -> T a
fromCement fromSign c = T mayCoe ex
  where
    Cement.T coe ex = c
    mayCoe = case coe of
      Coefficient.Zero -> Nothing
      Coefficient.NonZero (NovSign.T nd sgn) ->
        Just (fromSign sgn, nd)
