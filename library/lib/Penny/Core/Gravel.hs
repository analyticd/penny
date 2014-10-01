module Penny.Core.Gravel where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Cement as Cement
import qualified Penny.Core.Coeff as Coefficient
import Deka.Dec
import qualified Penny.Core.NovSign as NovSign
import qualified Penny.Core.Polarity as Polarity

-- | Components of a sided concrete type, such as 'Penny.Qty.T'.  The
-- type might be zero, in which case it has no coefficient and no
-- side; otherwise, it has both a coefficient and a side.  Whether it
-- is zero or not, it always has an exponent (this differentiates
-- between, for example, @0@ and @0.000@.)
data T a = T
  { exp :: Exp.T
  , polarity :: Polarity.T () NovDecs.T a
  } deriving (Eq, Ord, Show)

toCement :: (a -> Sign) -> T a -> Cement.T
toCement toSign (T e plrty) = Cement.T c e
  where
    c = case plrty of
      Polarity.Center _ -> Coefficient.Zero
      Polarity.OffCenter nd s -> Coefficient.NonZero
        $ NovSign.T nd (toSign s)

fromCement :: (Sign -> a) -> Cement.T -> T a
fromCement fromSign c = T ex plrty
  where
    Cement.T coe ex = c
    plrty = case coe of
      Coefficient.Zero -> Polarity.Center ()
      Coefficient.NonZero (NovSign.T nd sgn) ->
        Polarity.OffCenter nd (fromSign sgn)
