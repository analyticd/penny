module Penny.Core.Anna where

import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.Nil.Ungrouped as Nil.Ungrouped
import qualified Penny.Core.Anna.Nil.Grouped as Nil.Grouped
import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Anna.Radun as Radun
import qualified Penny.Core.Anna.RadZ as RadZ
import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Stokely as Stokely
import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Natural.Unsigned as Unsigned

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

toGravel :: T r -> Gravel.T ()
toGravel (Nil (Nil.Ungrouped (Nil.Ungrouped.LeadingZero
  (Znu1.T _ Nothing)))) = Gravel.T Nothing Exp.Zero

toGravel (Nil (Nil.Ungrouped (Nil.Ungrouped.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx Nothing)))))) = Gravel.T Nothing Exp.Zero

toGravel (Nil (Nil.Ungrouped (Nil.Ungrouped.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx (Just (Zeroes.T nz)))))))) =
  Gravel.T Nothing . Exp.fromUnsigned . Unsigned.fromNonZero $ nz

toGravel (Nil (Nil.Ungrouped (Nil.Ungrouped.NoLeadingZero
  (RadZ.T _rdx zs)))) =
  Gravel.T Nothing . Exp.fromUnsigned . Unsigned.fromNonZero
  . Zeroes.numDigits $ zs

toGravel (Nil (Nil.Grouped (Nil.Grouped.LeadingZero (Zng.T _
  (NG1.T _rdx zs1 _ zs2 zsgs))))) = undefined

