module Penny.Core.Anna where

import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Stokely as Stokely
import qualified Penny.Core.Polarity as Polarity

data T a
  = Nil (Nil.T a)
  | Brim (Brim.T a)
  deriving (Eq, Ord, Show)

-- | Given a polarity, transforms a 'T' to a 'Penny.Core.Stokeley.T'.
-- Fails if the 'T' is 'Nil'.

toStokelyPolar :: p -> T r -> Maybe (Stokely.T r p)
toStokelyPolar p t = case t of
  Nil _ -> Nothing
  Brim b -> Just . Stokely.T . Polarity.OffCenter b $ p

-- | Transforms a 'T' to a 'Penny.Core.Stokely.T', with no polarity.
-- Fails if the 'T' is 'Brim'.
toStokelyNonpolar :: T r -> Maybe (Stokely.T r p)
toStokelyNonpolar (Brim _) = Nothing
toStokelyNonpolar (Nil n) = Just . Stokely.T . Polarity.Center $ n
