module Penny.Lincoln.Cement where

import qualified Penny.Lincoln.Coeff as Coefficient
import qualified Penny.Lincoln.Exp as Exponent
import Prelude hiding (exponent)

-- | The two ingredients that define a concrete number.  Cement is not
-- the best name, as cement is only one of the ingredients of
-- concrete, but it will have to do.

data T = T
  { coefficient :: Coefficient.T
  , exponent :: Exponent.T
  } deriving (Eq, Ord, Show)

