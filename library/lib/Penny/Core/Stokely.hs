module Penny.Core.Stokely where

import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Core.CoeffExp as CoeffExp

-- | Number representations that may be neutral or, alternatively, may
-- be non-neutral.  The first type variable is the type of the radix
-- point and grouping character; see, for example,
-- "Penny.Core.Anna.RadCom" or "Penny.Core.Anna.RadPer".  The second
-- type variable is the polarity; see, for example, "Penny.Core.Side"
-- or "Penny.Core.PluMin".
newtype T r p
  = T { toPolarity :: Polarity.T (Nil.T r) (Brim.T r) p }
  deriving (Eq, Ord, Show)

toGravel :: T r p -> Gravel.T p
toGravel (T pty) = case pty of
  Polarity.Center nil -> Gravel.T (Nil.toExp nil) (Polarity.Center ())
  Polarity.OffCenter brim p -> Gravel.T ex (Polarity.OffCenter coe p)
    where
      CoeffExp.T coe ex = Brim.toCoeffExp brim
